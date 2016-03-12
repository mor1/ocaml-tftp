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

let src = Logs.Src.create "tftp" ~doc:"tftp-mirage"
module Log = (val Logs.src_log src : Logs.LOG)

let sp = Printf.sprintf

module S = struct

  module Make (FS: V1_LWT.KV_RO) (STACK: V1_LWT.STACKV4)
  = struct

    module U = STACK.UDPV4

    type t = {
      server: Tftp.S.t;
      fs: FS.t;
      s: STACK.t;
    }

    let handle_failure { server; fs; s } (sip,spt,dpt) =
      Tftp.Failure.(function
          | Unknown_tid -> Lwt.return_unit
          | File_not_found filename -> Lwt.return_unit
          | Invalid_packet -> Lwt.return_unit
          | Unsupported_mode mode -> Lwt.return_unit
          | Rrq_refused -> Lwt.return_unit
          | Unsupported_op opcode -> Lwt.return_unit
        )

    let handle_success { server; fs; s } (sip,spt,dpt) =
      Tftp.Success.(function
          | Packet p -> Lwt.return_unit
          | Retx (blockno, p) -> Lwt.return_unit
          | Ack_of_error -> Lwt.return_unit
          | Ack_of_eof -> Lwt.return_unit
          | Error (errorcode, msg) -> Lwt.return_unit
          | Request (filename, mode) -> Lwt.return_unit
        )

    let rec callback ~port t =
      let { server; s; _ } = t in
      Log.info (fun f -> f "Tftp: starting");
      (fun ~src ~dst ~src_port buf ->
         Log.info (fun f ->
           f "Tftp: rx %s.%d > %s.%d"
             (Ipaddr.V4.to_string src) src_port (Ipaddr.V4.to_string dst) port
         );

         Tftp.(match handle t.server src src_port port buf with
             | Ok (server, tid, success) ->
               Log.info (fun f -> f "Tftp: Ok: tid:%s server:%s"
                          (Tid.to_string tid) (Tftp.S.to_string server)
                       );

               handle_success t tid success
             | Fail (server, tid, outp, failure) ->
               Log.info (fun f -> f "Tftp: Fail: tid:%s server:%s failure:%s"
                          (Tid.to_string tid) (Tftp.S.to_string server)
                          "fail!"
                       );

               handle_failure t tid failure >>= fun () ->
               let (sip,spt,dpt) = tid in
               let source_port = dpt in
               let dest_ip = sip in
               let dest_port = spt in
               U.write ~source_port ~dest_ip ~dest_port (STACK.udpv4 s) outp
           )
      )
  end
end
