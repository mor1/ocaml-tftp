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

(** Server implementation of TFTP
    @see <http://tools.ietf.org/html/rfc1350>, "RFC 1350" *)

module Make(C:V1_LWT.CONSOLE)(FS:V1_LWT.KV_RO)(S:V1_LWT.STACKV4) : sig

  type t
  (** Server state record. *)

  val make: c:C.t -> fs:FS.t -> s:S.t -> unit -> t
  (** [make ~c ~fs ~s] creates a server state record from a {! V1_LWT.CONSOLE}
      for logging output, a {! V1_LWT.KV_RO} containing the files to serve, and
      a {! V1_LWT.STACKV4} IPv4 stack. *)

  val default_port: int
  (** Default listen port, [69/UDP]. Note that this is just for receiving the
      initial [RRQ]/[WRQ] -- data transfer will take place between a pair of
      ephemeral UDP ports. *)

  val callback: port:int -> t -> S.UDPV4.callback
  (** [callback ~port t] returns a {! S.UDPV4} callback handler listening on
      [port]. *)

end
