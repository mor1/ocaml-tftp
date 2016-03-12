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

let src = Logs.Src.create "unikernel" ~doc:"Main unikernel code"
module Log = (val Logs.src_log src : Logs.LOG)

module Main (C: V1.CLOCK) (FS: V1_LWT.KV_RO) (S: V1_LWT.STACKV4) = struct
  module Logs_reporter = Mirage_logs.Make(Clock)

  module T = Tftp_mirage.S.Make(FS)(S)

  let start () fs s =
    Logs.(set_level (Some Info));
    Logs_reporter.(create () |> run) @@ fun () ->
    let files = "./files" in
    let config = Tftp_config.make files in
    let port = Tftp_config.port config in
    let server = Tftp.S.make config in
    S.listen_udpv4 s ~port T.(callback ~port { server; fs; s });
    S.listen s
end
