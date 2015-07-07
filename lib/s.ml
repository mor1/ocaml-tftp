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

module Make(C:V1_LWT.CONSOLE)(FS:V1_LWT.KV_RO)(S:V1_LWT.STACKV4) = struct

  module U = S.UDPV4

  type tid = U.ipaddr * int * int
  let tid_to_string (rip,rpt, lpt) =
    sp "(%s,%d, %d)" (Ipaddr.V4.to_string rip) rpt lpt

  type t = {
    c: C.t;
    fs: FS.t;
    s: S.t;

    conns: (tid, string * int64 * int64) Hashtbl.t;
    tids: (U.ipaddr * int, int) Hashtbl.t;
    files: (string, Cstruct.t) Hashtbl.t;
  }

  let default_port = 69
  let min_port = 32768

  let make ~c ~fs ~s () =
    let conns = Hashtbl.create 16 in
    let tids = Hashtbl.create 16 in
    let files = Hashtbl.create 16 in
    { c; fs; s; conns; tids; files }

  let error { c; _ } msg =
    let msg = sp "ERROR: %s" msg in
    C.log_s c msg >>= fun () -> Lwt.fail (Failure msg)

  let obuf len = Cstruct.set_len (Io_page.get_buf ~n:1 ()) len

  let tx_errp ~t ~tid e msg =
    let msglen = String.length msg in
    let obuf = obuf (Wire.sizeof_herr + msglen + 1) in
    Wire.(set_herr_opcode obuf (opcode_to_int ERROR));
    Wire.(set_herr_errorcode obuf (errorcode_to_int e));
    Cstruct.blit_from_string msg 0 obuf Wire.sizeof_herr msglen;
    Cstruct.set_char obuf (Wire.sizeof_herr + msglen) '\x00';
    let (dest_ip, dest_port, source_port) = tid in
    U.write ~source_port ~dest_ip ~dest_port (S.udpv4 t.s) obuf

  let tx_datap ~t ~tid filename =
    Hashtbl.get t.conns tid
    |> function
    | None ->
      C.log t.c (sp "tx_datap! unknown tid %s" filename);
      tx_errp ~t ~tid Wire.UNKNOWN_TID (tid_to_string tid)

    | Some (filename, filesize, blockno) ->
      Hashtbl.get t.files filename
      |> function
      | None ->
        C.log t.c (sp "tx_datap! file not found %s" filename);
        tx_errp ~t ~tid Wire.FILE_NOT_FOUND filename

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
        U.write ~source_port ~dest_ip ~dest_port (S.udpv4 t.s) obuf

  (** *)

  let handle_rrq t tid buf =
    C.log t.c (sp "RRQ %s" (tid_to_string tid));
    let (sip, spt, _) = tid in
    let (filename, buf) = Cstruct.shift buf Wire.sizeof_hreq |> Wire.string0 in
    C.log t.c (sp "filename=%s" filename);

    let (mode, _buf) = Wire.string0 buf in

    match mode with
    | "octet" -> (
        FS.size t.fs filename >>= (function
            | `Error (FS.Unknown_key s) -> error t (sp "unknown key %s" s)
            | `Ok filesize -> Lwt.return filesize
          )

        >>= fun filesize ->
        Hashtbl.get t.tids ~default:min_port (sip,spt)
        |> function
        | None ->
          C.log t.c (sp "rrq! failed to get tid (%s,%d)"
                       (Ipaddr.V4.to_string sip) spt);
          tx_errp ~t ~tid Wire.UNDEFINED (sp "failed to get tid %d" spt)
          >>= fun () -> Lwt.return_none

        | Some source_port ->
          FS.read t.fs filename 0 (Int64.to_int filesize) >>= (function
              | `Error (FS.Unknown_key s) ->
                error t (sp "read! unknown key %s" s)
              | `Ok pages ->
                let data = Cstruct.(pages |> copyv |> of_string) in
                let tid = (sip,spt, source_port) in
                Hashtbl.replace t.files filename data;
                Hashtbl.replace t.conns tid (filename, filesize, 0L);
                Hashtbl.replace t.tids (sip,spt) (source_port + 1);
                Lwt.return tid
            )
          >>= fun tid -> tx_datap ~t ~tid filename
          >>= fun () ->
          Lwt.return (Some tid)
      )
    | _ ->
      C.log t.c (sp "rrq! unsupported mode %s" mode);
      Hashtbl.replace t.conns tid (filename, 0L, -1L);
      tx_errp ~t ~tid Wire.ILLEGAL_OP (sp "mode:%s" mode)
      >>= fun () -> Lwt.return_none

  let handle_ack t tid buf =
    let (sip, spt, dpt) = tid in
    let ackno = Wire.get_hdat_blockno buf |> Int64.of_int in
    Hashtbl.get t.conns tid
    |> function
    | None ->
      C.log t.c (sp "ack! unknown tid %s" (tid_to_string tid));
      tx_errp ~t ~tid Wire.UNKNOWN_TID (tid_to_string tid)

    | Some (filename, filesize, blockno) ->
      C.log t.c
        (sp "ACK: filename=%s ackno=%Ld blockno=%Ld" filename ackno blockno);

      if blockno < 0L then (
        C.log t.c "ACK: of errp";
        Hashtbl.remove t.conns (sip,spt,dpt);
        Lwt.return_unit

      ) else if ackno < blockno then (
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

  let unhandled t tid opcode =
    C.log t.c (sp "%s unhandled!" (Wire.opcode_to_string opcode));
    tx_errp ~t ~tid Wire.ILLEGAL_OP (Wire.opcode_to_string opcode)

  let rec callback port t =
    let { c; _ } = t in
    C.log c "Tftp: starting";
    (fun ~src ~dst ~src_port buf ->
       C.log c
         (sp "Tftp: rx %s.%d > %s.%d"
            (Ipaddr.V4.to_string src) src_port (Ipaddr.V4.to_string dst) port
         );

       Wire.(buf |> get_hreq_opcode |> int_to_opcode |> function
         | None -> error t (Cstruct.debug buf)
         | Some o -> match o with
           | RRQ -> (
               handle_rrq t (src,src_port, port) buf
               >>= function
               | None -> Lwt.return_unit
               | Some (_rip,_rpt, port)  ->
                 S.listen_udpv4 t.s ~port (callback port t);
                 Lwt.return_unit
             )
           | ACK -> handle_ack t (src,src_port, port) buf
           | ERROR -> handle_error t buf
           | WRQ | DATA -> unhandled t (src, src_port, port) o
         )
    )

end
