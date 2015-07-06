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

module Make(C:V1_LWT.CONSOLE)(FS:V1_LWT.KV_RO)(U:V1_LWT.UDPV4) = struct


  type t = {
    port: int;
    c: C.t;
    fs: FS.t;
    u: U.t;

    conns: (U.ipaddr * int, int) Hashtbl.t;
    files: (U.ipaddr * int, Cstruct.t) Hashtbl.t;
  }

  let default_port = 69

  let make ?(port=default_port) ~c ~fs ~u () =
    let conns = Hashtbl.create 16 in
    let files = Hashtbl.create 16 in
    { port; c; fs; u; conns; files }

  let error { c; _ } msg =
    let msg = sp "ERROR: %s" msg in
    C.log_s c msg >>= fun () -> Lwt.fail (Failure msg)

  let read_file t n =
    FS.size t.fs n >>= function
    | `Error (FS.Unknown_key _) -> error t (sp "unknown key: n=%s" n)
    | `Ok size ->
      FS.read t.fs n 0 (Int64.to_int size) >>= function
      | `Error (FS.Unknown_key _) -> error t (sp "unknown key: n=%s" n)
      | `Ok bufs -> bufs |> Cstruct.copyv |> Lwt.return

  let datap ~blockno data =
    let srcoff = (blockno*512) - 512 in
    let datlen = min 512 ((Cstruct.len data)-srcoff) in
    let buf =
      Cstruct.(set_len (Io_page.get_buf ~n:1 ()) (Wire.sizeof_hdat + datlen))
    in
    Wire.(set_hdat_opcode buf (opcode_to_int DATA));
    Wire.(set_hdat_blockno buf blockno);
    Cstruct.blit data srcoff buf Wire.sizeof_hdat datlen;
    buf

  let handle_rrq t ((sip,spt), (dip,dpt)) buf =
    C.log_s t.c "RRQ" >>= fun () ->

    let (filename, buf) = Cstruct.shift buf Wire.sizeof_hreq |> Wire.string0 in
    C.log_s t.c (sp "filename=%s" filename) >>= fun () ->

    read_file t filename >|= Cstruct.of_string
    >>= (fun data ->
        Hashtbl.add t.conns (sip,spt) 1;
        Hashtbl.add t.files (sip,spt) data;
        let obuf = datap ~blockno:1 data in
        U.write ~source_port:t.port ~dest_ip:sip ~dest_port:spt t.u obuf
      )

  let handle_ack t ((sip,spt), (dip,dpt)) buf =
    let data =
      try Some (Hashtbl.find t.files (sip,spt))
      with
      | Not_found -> C.log t.c "ACK: file not found!"; None
    in
    let blockno =
      try Some (Hashtbl.find t.conns (sip,spt))
      with
      | Not_found -> C.log t.c "ACK: conn not found!"; None
    in
    let ackno = Wire.get_hdat_blockno buf in
    match data, blockno with
    | Some data, Some blockno -> (
        C.log t.c (sp "ACK: ackno=%d blockno=%d" ackno blockno);
        if ackno != blockno then
          C.log_s t.c "ACK: unexpected ackno!"
        else if blockno * 512 <= Cstruct.len data then (
          C.log t.c "ACK: ok!";
          let blockno = blockno + 1 in
          Hashtbl.replace t.conns (sip,spt) blockno;
          let obuf = datap ~blockno data in
          U.write ~source_port:t.port ~dest_ip:sip ~dest_port:spt t.u obuf
        )
        else (
          C.log t.c "ACK: end-of-file!";
          Hashtbl.remove t.conns (sip,spt);
          Hashtbl.remove t.files (sip,spt);
          Lwt.return_unit
        )
      )
    | _, _ -> C.log_s t.c "ACK: never reached!"


  let unhandled { c; _ } opcode =
    C.log_s c (sp "%s unhandled!" (Wire.opcode_to_string opcode))

  let handle_error { c; _ } _buf =
    C.log_s c "ERROR"

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
           | WRQ | DATA -> unhandled t o
           | ERROR -> handle_error t buf
         )
    )

end
