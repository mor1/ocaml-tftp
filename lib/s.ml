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

module Make(C:V1_LWT.CONSOLE)(U:V1_LWT.UDPV4) = struct

  type t = {
    port: int;
    console: C.t;
  }

  let default_port = 69
  let port { port; _ } = port
  let set_port p t = { t with port=p }

  let console { console; _ } = console
  let set_console c t = { t with console=c }

  let make ?(port=default_port) ~console = { port; console }

  let error c buf =
    C.log_s c (sp "ERROR: opcode=%d" (Wire.get_h_opcode buf))

  let handle_rrq c buf =
    C.log_s c "RRQ" >>= fun () ->
    let (filename, buf) = Wire.string0 buf in
    C.log_s c (sp "filename=%s" filename)

  let handle_wrq c _buf =
    C.log_s c "WRQ"

  let handle_data c _buf =
    C.log_s c "DATA"

  let handle_ack c _buf =
    C.log_s c "ACK"

  let handle_error c _buf =
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
         | None -> error c buf
         | Some o ->
           let buf = Cstruct.shift buf sizeof_h in
           match o with
           | RRQ -> handle_rrq c buf
           | WRQ -> handle_wrq c buf
           | DATA -> handle_data c buf
           | ACK -> handle_ack c buf
           | ERROR -> handle_error c buf
         )
    )

end
