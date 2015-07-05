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

open Mirage
open Lwt.Infix

let sp = Printf.sprintf

module Make(C:V1_LWT.CONSOLE)(FS:V1_LWT.KV_RO)(U:V1_LWT.UDPV4) = struct

  type t = {
    port: int;
    c: C.t;
    fs: FS.t;
  }

  let default_port = 69
  let port { port; _ } = port
  let set_port port t = { t with port }

  let console { c; _ } = c
  let set_console c t = { t with c }

  let make ?(port=default_port) ~c ~fs () = { port; c; fs }

  let error { c; _ } msg =
    let msg = sp "ERROR: %s" msg in
    C.log_s c msg >>= fun () -> Lwt.fail (Failure msg)

  let read_file t n =
    let { c; fs; _ } = t in
    FS.size fs n >>= function
    | `Error (FS.Unknown_key _) -> error t (sp "unknown key: n=%s" n)
    | `Ok size ->
      FS.read fs n 0 (Int64.to_int size) >>= function
      | `Error (FS.Unknown_key _) -> error t (sp "unknown key: n=%s" n)
      | `Ok bufs -> Lwt.return (Cstruct.copyv bufs)

  let handle_rrq t buf =
    let { c; fs; _ } = t in
    C.log_s c "RRQ" >>= fun () ->

    let (filename, buf) = Wire.string0 buf in
    C.log_s c (sp "filename=%s" filename) >>= fun () ->

    read_file t filename >>= fun file ->
    error t file

  let handle_wrq { c; _ } _buf =
    C.log_s c "WRQ"

  let handle_data { c; _ } _buf =
    C.log_s c "DATA"

  let handle_ack { c; _ } _buf =
    C.log_s c "ACK"

  let handle_error { c; _ } _buf =
    C.log_s c "ERROR"

  let callback t =
    let c = console t in
    C.log c "Tftp: starting";
    (fun ~src ~dst ~src_port buf ->
       C.log c
         (sp "Tftp: rx %s.%d -> %s.%d"
            (Ipaddr.V4.to_string src) src_port
            (Ipaddr.V4.to_string dst) (port t)
         );

       Wire.(buf |> get_h_opcode |> int_to_opcode |> function
         | None -> error t (Cstruct.debug buf)
         | Some o ->
           let buf = Cstruct.shift buf sizeof_h in
           match o with
           | RRQ -> handle_rrq t buf
           | WRQ -> handle_wrq t buf
           | DATA -> handle_data t buf
           | ACK -> handle_ack t buf
           | ERROR -> handle_error t buf
         )
    )

end
