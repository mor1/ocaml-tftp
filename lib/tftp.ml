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

module Config = struct

  type t = {
    port: int;
    files: string;
  } with sexp

  let port t = t.port

  let default_port = 69
  let min_port = 32768

end

type server = {
  port: int;
  conns: (Tid.t, string * int64 * int64) Hashtbl.t;
  tids: (Ipaddr.V4.t * int, int) Hashtbl.t;
  files: (string, int64 * Cstruct.t) Hashtbl.t;
} with sexp

let server config =
  let port = Config.port config in
  let conns = Hashtbl.create 16 in
  let tids = Hashtbl.create 16 in
  let files = Hashtbl.create 16 in
  { port; conns; tids; files }

type mode = Octet | Netascii | Mail

type success = [
  | `Packet of Cstruct.t
  | `Retx of int64 * Cstruct.t
  | `Ack_of_error
  | `Ack_of_eof
  | `Error of Wire.errorcode * string
  | `Request of string * mode
]

type failure = [
  | `Unknown_tid
  | `File_not_found of string
  | `Invalid_packet
  | `Unsupported_mode of string
  | `Rrq_refused
  | `Unsupported_op of Wire.opcode
]

type ret =
  | Ok of Tid.t * success
  | Fail of Tid.t * failure * Cstruct.t

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
  Hashtbl.get server.files filename |> function
  | None ->
    let errp = errorp ~msg:filename Wire.FILE_NOT_FOUND in
    Fail (tid, `File_not_found filename, errp)
  | Some (filesize, data) ->
    let srcoff = Int64.(mul blockno 512L) in
    let datlen = Int64.(to_int (min 512L (sub filesize srcoff))) in
    let obuf = obuf (Wire.sizeof_hdat + datlen) in
    Wire.(set_hdat_opcode obuf (opcode_to_int DATA));
    Wire.(set_hdat_blockno obuf (Int64.to_int blockno));
    Cstruct.blit data (Int64.to_int srcoff) obuf Wire.sizeof_hdat datlen;
    Ok (tid, f obuf)

let handle_ack server tid (filename, filesize, blockno) inp =
  let ackno = Wire.get_hdat_blockno inp |> Int64.of_int in
  if blockno < 0L then
    Ok (tid, `Ack_of_error)
  else if ackno < blockno then
    datap server tid filename ackno (fun p -> `Retx (ackno, p))
  else if filesize < Int64.mul blockno 512L then
    Ok (tid, `Ack_of_eof)
  else datap server tid filename blockno (fun p -> `Packet p)

let handle_error server tid inp =
  let error = Wire.(
      inp |> get_herr_errorcode |> Wire.int_to_errorcode |> function
      | None -> UNDEFINED
      | Some c -> c
    )
  in
  let (msg, _) = Cstruct.shift inp Wire.sizeof_herr |> Wire.string0 in
  Ok (tid, `Error (error, msg))

let handle_rrq server tid inp =
  let (filename, inp) = Cstruct.shift inp Wire.sizeof_hreq |> Wire.string0 in
  let (mode, _inp) = Wire.string0 inp in
  match mode with
  | "octet" ->
    if not (Hashtbl.mem server.files filename) then
      let errp = errorp ~msg:filename Wire.FILE_NOT_FOUND in
      Fail (tid, `File_not_found filename, errp)
    else (
      let (sip, spt, _tftp_port) = tid in
      Hashtbl.get server.tids ~default:Config.min_port (sip,spt) |> function
      | None ->
        let errp = errorp ~msg:("no TID for "^filename) Wire.UNDEFINED in
        Fail (tid, `Rrq_refused, errp)
      | Some local_port ->
        Ok ((sip,spt, local_port), `Request (filename, Octet))
    )
  | mode ->
    Fail (tid, `Unsupported_mode mode,
          errorp ~msg:("unsupported mode:"^mode) Wire.ILLEGAL_OP)

let handle server sip spt dpt inp =
  let tid = Tid.(sip, spt, dpt) in
  Hashtbl.get server.conns tid |> function
  | None ->
    Fail (tid, `Unknown_tid, errorp ~msg:(Tid.to_string tid) Wire.UNKNOWN_TID)
  | Some conn ->
    Wire.(inp |> get_hreq_opcode |> int_to_opcode |> function
      | None -> Fail (tid, `Invalid_packet, inp)
      | Some o -> match o with
        | ACK -> handle_ack server tid conn inp
        | ERROR -> handle_error server tid inp
        | RRQ -> handle_rrq server tid inp
        | WRQ | DATA ->
          let errp = errorp ~msg:(Wire.opcode_to_string o) Wire.ILLEGAL_OP in
          Fail (tid, `Unsupported_op o, errp)
      )
