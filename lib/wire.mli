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

(** {1 TFTP wire parsers} *)

(** {2 TFTP error codes} *)

type errorcode =
  | UNDEFINED         (** operation undefined, e.g., couldn't create [TID] *)
  | FILE_NOT_FOUND    (** file not found in store *)
  | ACCESS_VIOLATION
  | DISK_FULL
  | ILLEGAL_OP        (** unsupported operation, e.g., unsupported [mode] *)
  | UNKNOWN_TID
  | FILE_EXISTS
  | UNKNOWN_USER

(** Conversion to/from {! errorcode}s. *)

val int_to_errorcode : int -> errorcode option
val errorcode_to_int : errorcode -> int
val errorcode_to_string : errorcode -> string
val string_to_errorcode : string -> errorcode option

(** {2 TFTP opcodes} *)

type opcode =
  | RRQ                         (** read request *)
  | WRQ                         (** write request *)
  | DATA                        (** data block *)
  | ACK                         (** acknowledgement *)
  | ERROR                       (** error indication *)

(** Conversion to/from {! opcode}s. *)

val int_to_opcode : int -> opcode option
val opcode_to_int : opcode -> int
val opcode_to_string : opcode -> string
val string_to_opcode : string -> opcode option

(** {2 Request packets, {! opcode.RRQ}/{! opcode.WRQ}} *)

val sizeof_hreq : int
val get_hreq_opcode : Cstruct.t -> Cstruct.uint16
val set_hreq_opcode : Cstruct.t -> Cstruct.uint16 -> unit
val hexdump_hreq_to_buffer : Buffer.t -> Cstruct.t -> unit
val hexdump_hreq : Cstruct.t -> unit

(** {2 Data packets, {! opcode.DATA}/{! opcode.ACK}} *)

val sizeof_hdat : int
val get_hdat_opcode : Cstruct.t -> Cstruct.uint16
val set_hdat_opcode : Cstruct.t -> Cstruct.uint16 -> unit
val get_hdat_blockno : Cstruct.t -> Cstruct.uint16
val set_hdat_blockno : Cstruct.t -> Cstruct.uint16 -> unit
val hexdump_hdat_to_buffer : Buffer.t -> Cstruct.t -> unit
val hexdump_hdat : Cstruct.t -> unit

(** {2 Error packets, {! opcode.ERROR}} *)

val sizeof_herr : int
val get_herr_opcode : Cstruct.t -> Cstruct.uint16
val set_herr_opcode : Cstruct.t -> Cstruct.uint16 -> unit
val get_herr_errorcode : Cstruct.t -> Cstruct.uint16
val set_herr_errorcode : Cstruct.t -> Cstruct.uint16 -> unit
val hexdump_herr_to_buffer : Buffer.t -> Cstruct.t -> unit
val hexdump_herr : Cstruct.t -> unit

(** {2 Utility functions} *)

(** [string0 buf] extracts a [NUL] terminated ASCII string from [buf], shifts
    [buf] past it, and returns the pair. *)
val string0 : Cstruct.t -> string * Cstruct.t
