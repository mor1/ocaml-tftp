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

open Cstruct

cenum errorcode {
    UNDEF = 0;
    FILE_NOT_FOUND;
    ACCESS_VIOLATION;
    DISK_FULL;
    ILLEGAL_OP;
    UNKNOWN_TID;
    FILE_EXISTS;
    UNKNOWN_USER
  } as uint8_t

cenum opcode {
    RRQ = 1;
    WRQ;
    DATA;
    ACK;
    ERROR
  } as uint16_t

cstruct h {
    uint16_t opcode
  } as big_endian

cstruct blockno {
    uint16_t blockno
  } as big_endian

let string0 buf =
  let rec aux s i buf =
    let c = Cstruct.get_char buf i in
    if c = '\x00' then
      let string = s |> List.rev |> String.concat "/" in
      (string, Cstruct.shift buf (i+1))
    else
      aux (Char.escaped c :: s) (i+1) buf
  in
  aux [] 0 buf
