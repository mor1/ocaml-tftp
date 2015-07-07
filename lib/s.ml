(*
 * Copyright (c) 2015 Richard Mortier <mort@cantab.net>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Lwt.Infix

let sp = Printf.sprintf

module Hashtbl = struct
  include Hashtbl

  let get t ?default k =
    try Some (Hashtbl.find t k)
    with
    | Not_found ->
      (match default with None -> () | Some v -> Hashtbl.replace t k v);
      default

end

module Make(C:V1_LWT.CONSOLE)(FS:V1_LWT.KV_RO)(U:V1_LWT.UDPV4) = struct

  type tid = U.ipaddr * int * int
  let tid_to_string (rip,rpt, lpt) =
    sp "(%s,%d, %d)" (Ipaddr.V4.to_string rip) rpt lpt

  type t = {
    port: int;
    c: C.t;
    fs: FS.t;
    u: U.t;

    tids: (U.ipaddr * int, int) Hashtbl.t;
    files: (string, Cstruct.t) Hashtbl.t;
    conns: (tid, string * int64 * int64) Hashtbl.t;
  }

  let default_port = 69
  let min_port = 32768

  let make ?(port=default_port) ~c ~fs ~u () =
    let conns = Hashtbl.create 16 in
    let tids = Hashtbl.create 16 in
    let files = Hashtbl.create 16 in
    { port; c; fs; u; conns; tids; files }

  let error { c; _ } msg =
    let msg = sp "ERROR: %s" msg in
    C.log_s c msg >>= fun () -> Lwt.fail (Failure msg)

  let tx_datap ~t ~tid filename =
    Hashtbl.get t.conns tid
    |> function
    | None -> C.log_s t.c (sp "tx_datap! unknown tid %s" filename)

    | Some (filename, filesize, blockno) ->
      Hashtbl.get t.files filename
      |> function
      | None -> C.log_s t.c (sp "tx_datap! unknown filename %s" filename)

      | Some data ->
        let srcoff = Int64.(mul blockno 512L) in
        let datlen = Int64.(to_int (min 512L (sub filesize srcoff))) in
        let blockno = Int64.add blockno 1L in
        let obuf =
          let buf = Io_page.get_buf ~n:1 () in
          Cstruct.set_len buf (Wire.sizeof_hdat + datlen)
        in
        Wire.(set_hdat_opcode obuf (opcode_to_int DATA));
        Wire.(set_hdat_blockno obuf (Int64.to_int blockno));
        Hashtbl.replace t.conns tid (filename, filesize, blockno);
        Cstruct.blit
          data (Int64.to_int srcoff) obuf Wire.sizeof_hdat datlen;
        let (dest_ip, dest_port, source_port) = tid in
        U.write ~source_port ~dest_ip ~dest_port t.u obuf

  (** *)

  let handle_rrq t ((sip,spt), (dip,dpt)) buf =
    C.log t.c "RRQ";
    let (filename, buf) = Cstruct.shift buf Wire.sizeof_hreq |> Wire.string0 in
    C.log t.c (sp "filename=%s" filename);

    FS.size t.fs filename >>= (function
        | `Error (FS.Unknown_key s) -> error t (sp "unknown key %s" s)
        | `Ok filesize -> Lwt.return filesize
      )

    >>= fun filesize ->
    Hashtbl.get t.tids ~default:min_port (sip,spt)
    |> function
    | None -> C.log_s t.c (sp "rrq! failed to get tid (%s,%d)"
                             (Ipaddr.V4.to_string sip) spt)

    | Some source_port ->
      FS.read t.fs filename 0 (Int64.to_int filesize) >>= (function
          | `Error (FS.Unknown_key s) -> error t (sp "read! unknown key %s" s)
          | `Ok pages ->
            let data = Cstruct.(pages |> copyv |> of_string) in
            let loc_port = source_port + 1 in
            Hashtbl.replace t.tids (sip,spt) loc_port;
            let tid = (sip,spt, loc_port) in
            Hashtbl.replace t.files filename data;
            Hashtbl.replace t.conns tid (filename, filesize, 0L);
            Lwt.return tid
        )

      >>= fun tid ->
      tx_datap ~t ~tid filename

  let handle_ack t ((sip,spt), (dip,dpt)) buf =
    let tid = (sip, spt, dpt) in
    let ackno = Wire.get_hdat_blockno buf |> Int64.of_int in
    Hashtbl.get t.conns tid
    |> function
    | None -> C.log_s t.c (sp "ack! unknown tid %s" (tid_to_string tid))

    | Some (filename, filesize, blockno) ->
      C.log t.c
        (sp "ACK: filename=%s ackno=%Ld blockno=%Ld" filename ackno blockno);

      if ackno < blockno then (
        C.log t.c (sp "ACK: retx of %Ld requested!" ackno);
        Lwt.return_unit

      ) else if filesize < Int64.mul blockno 512L then (
        C.log t.c "ACK: end-of-file!";
        Hashtbl.remove t.conns (sip,spt,dpt);
        Lwt.return_unit

      ) else (
        C.log t.c "ACK: ok!";
        tx_datap ~t ~tid filename

      )

  let handle_error { c; _ } _buf =
    C.log_s c "ERROR"

  let unhandled { c; _ } opcode =
    C.log_s c (sp "%s unhandled!" (Wire.opcode_to_string opcode))

  let callback t =
    let { port; c; _ } = t in
    C.log c "Tftp: starting";
    (fun ~src ~dst ~src_port buf ->
       C.log c
         (sp "Tftp: rx %s.%d -> %s.%d"
            (Ipaddr.V4.to_string src) src_port
            (Ipaddr.V4.to_string dst) port
         );

       Wire.(buf |> get_hreq_opcode |> int_to_opcode |> function
         | None -> error t (Cstruct.debug buf)
         | Some o -> match o with
           | RRQ -> handle_rrq t ((src,src_port), (dst,t.port)) buf
           | ACK -> handle_ack t ((src,src_port), (dst,t.port)) buf
           | ERROR -> handle_error t buf
           | WRQ | DATA -> unhandled t o
         )
    )

end
