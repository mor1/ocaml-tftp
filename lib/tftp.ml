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

open Sexplib.Std
open Sexplib.Conv

module Wire = Tftp_wire

module Hashtbl = struct
  include Hashtbl

  let get t ?default k =
    try Some (Hashtbl.find t k)
    with
    | Not_found ->
      (match default with None -> () | Some v -> Hashtbl.replace t k v);
      default

end

module Tid = struct

  type t = Ipaddr.V4.t * int * int with sexp
  (** source IP, source port, destination port *)

  let to_string t = t |> sexp_of_t |> Sexplib.Sexp.to_string_hum

end

module S = struct

  type t = {
    port: int;
    conns: (Tid.t, string * int64 * int64) Hashtbl.t;
    tids: (Ipaddr.V4.t * int, int) Hashtbl.t;
    files: (string, int64 * Cstruct.t) Hashtbl.t;
  } with sexp

  let to_string t = t |> sexp_of_t |> Sexplib.Sexp.to_string_hum

  let make config =
    let port = Config.port config in
    let conns = Hashtbl.create 16 in
    let tids = Hashtbl.create 16 in
    let files = Hashtbl.create 16 in
    { port; conns; tids; files }

end

type mode =
  | Octet
  | Netascii
  | Mail
  | Unknown of string
with sexp

let mode_of_string = function
  | "octet" -> Octet
  | "netascii" -> Netascii
  | "mail" -> Mail
  | mode -> Unknown mode

let mode_to_string = function
  | Octet -> "octet"
  | Netascii -> "netascii"
  | Mail -> "mail"
  | Unknown mode -> mode

module Success = struct
  type t =
    | Packet of Cstruct.t
    | Retx of int64 * Cstruct.t
    | Ack_of_error
    | Ack_of_eof
    | Error of Tftp_wire.errorcode * string
    | Request of string * mode
  with sexp

  let to_string t = t |> sexp_of_t |> Sexplib.Sexp.to_string_hum
end

module Failure = struct
  type t =
    | Unknown_tid
    | File_not_found of string
    | Invalid_packet
    | Unsupported_mode of mode
    | Rrq_refused
    | Unsupported_op of Tftp_wire.opcode
  with sexp

  let to_string t = t |> sexp_of_t |> Sexplib.Sexp.to_string_hum
end

type ret =
  | Ok of S.t * Tid.t * Success.t
  | Fail of S.t * Tid.t * Cstruct.t * Failure.t

let obuf len = Cstruct.set_len (Io_page.get_buf ~n:1 ()) len

let errorp ?msg error =
  let msg = match msg with
    | None -> Wire.errorcode_to_string error
    | Some m -> m
  in
  let msglen = String.length msg in
  let obuf = obuf (Wire.sizeof_herr + msglen + 1) in
  Wire.(set_herr_opcode obuf (opcode_to_int ERROR));
  Wire.(set_herr_errorcode obuf (errorcode_to_int error));
  Cstruct.blit_from_string msg 0 obuf Wire.sizeof_herr msglen;
  Cstruct.set_char obuf (Wire.sizeof_herr + msglen) '\x00';
  obuf

let datap server tid filename blockno f =
  Hashtbl.get server.S.files filename |> function
  | None ->
    let errp = errorp ~msg:filename Wire.FILE_NOT_FOUND in
    Fail (server, tid, errp, Failure.File_not_found filename)
  | Some (filesize, data) ->
    let srcoff = Int64.(mul blockno 512L) in
    let datlen = Int64.(to_int (min 512L (sub filesize srcoff))) in
    let obuf = obuf (Wire.sizeof_hdat + datlen) in
    Wire.(set_hdat_opcode obuf (opcode_to_int DATA));
    Wire.(set_hdat_blockno obuf (Int64.to_int blockno));
    Cstruct.blit data (Int64.to_int srcoff) obuf Wire.sizeof_hdat datlen;
    Ok (server, tid, f obuf)

let handle_ack server tid inp =
  Hashtbl.get server.S.conns tid |> function
  | None ->
    let errp = errorp ~msg:(Tid.to_string tid) Wire.UNKNOWN_TID in
    Fail (server, tid, errp, Failure.Unknown_tid)
  | Some (filename, filesize, blockno) ->
    let ackno = Wire.get_hdat_blockno inp |> Int64.of_int in
    if blockno < 0L then
      Ok (server, tid, Success.Ack_of_error)
    else if ackno < blockno then
      datap server tid filename ackno (fun p -> Success.Retx (ackno, p))
    else if filesize < Int64.mul blockno 512L then
      Ok (server, tid, Success.Ack_of_eof)
    else
      datap server tid filename blockno (fun p -> Success.Packet p)

let handle_error server tid inp =
  let error = Wire.(
      inp |> get_herr_errorcode |> Wire.int_to_errorcode |> function
      | None -> UNDEFINED
      | Some c -> c
    )
  in
  let (msg, _) = Cstruct.shift inp Wire.sizeof_herr |> Wire.string0 in
  Ok (server, tid, Success.Error (error, msg))

let handle_rrq server tid inp =
  let (filename, inp) = Cstruct.shift inp Wire.sizeof_hreq |> Wire.string0 in
  let (mode, _inp) = Wire.string0 inp in
  match mode_of_string mode with
  | Unknown m as mode ->
    let errp = errorp ~msg:"unknown mode" Wire.ILLEGAL_OP in
    Fail (server, tid, errp, Failure.Unsupported_mode mode)

  | Octet ->
    if not (Hashtbl.mem server.S.files filename) then
      let errp = errorp ~msg:filename Wire.FILE_NOT_FOUND in
      Fail (server, tid, errp, Failure.File_not_found filename)
    else (
      let (sip, spt, _tftp_port) = tid in
      Hashtbl.get server.S.tids ~default:Config.min_port (sip,spt) |> function
      | None ->
        let errp = errorp ~msg:"TID failure" Wire.UNDEFINED in
        Fail (server, tid, errp, Failure.Rrq_refused)
      | Some local_port ->
        let tid = (sip,spt, local_port) in
        Ok (server, tid, Success.Request (filename, Octet))
    )
  | mode ->
    let errp = errorp ~msg:"unsupported mode" Wire.ILLEGAL_OP in
    Fail (server, tid, errp, Failure.Unsupported_mode mode)

let handle server sip spt dpt inp =
  let tid = Tid.(sip, spt, dpt) in
  Wire.(inp |> get_hreq_opcode |> int_to_opcode |> function
    | None -> Fail (server, tid, inp, Failure.Invalid_packet)
    | Some o -> match o with
      | ACK -> handle_ack server tid inp
      | ERROR -> handle_error server tid inp
      | RRQ -> handle_rrq server tid inp
      | WRQ | DATA ->
        let errp = errorp ~msg:(Wire.opcode_to_string o) Wire.ILLEGAL_OP in
        Fail (server, tid, errp, Failure.Unsupported_op o)
    )
