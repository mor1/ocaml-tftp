(*
 * Copyright (c) 2015 Richard Mortier <mort@cantab.net>
 * Copyright (c) 2015 Thomas Gazagnaire <thomas@gazagnaire.org>
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

let err fmt =
  Printf.ksprintf (fun str ->
      Printf.eprintf ("\027[31m[ERROR]\027[m     %s\n") str;
      exit 1
    ) fmt

let env_info fmt = Printf.printf ("\027[33mENV\027[m         " ^^ fmt ^^ "\n%!")

let split c s =
  let rec aux c s ri acc =
    (* half-closed intervals. [ri] is the open end, the right-fencepost.
       [li] is the closed end, the left-fencepost. either [li] is
       + negative (outside [s]), or
       + equal to [ri] ([c] not found in remainder of [s]) ->
         take everything from [ s[0], s[ri] )
       + else inside [s], thus an instance of the separator ->
         accumulate from the separator to [ri]: [ s[li+1], s[ri] )
         and move [ri] inwards to the discovered separator [li]
    *)
    let li = try String.rindex_from s (ri-1) c with Not_found -> -1 in
    if li < 0 || li == ri then (String.sub s 0 ri) :: acc
    else begin
      let len = ri-1 - li in
      let rs = String.sub s (li+1) len in
      aux c s li (rs :: acc)
    end
  in
  aux c s (String.length s) []

let ips_of_env x = split ':' x |> List.map Ipaddr.V4.of_string_exn
let bool_of_env = function "1" | "true" | "yes" -> true | _ -> false
let net_of_env = function "socket" -> `Socket | _ -> `Direct
let fs_of_env = function "fat" -> `Fat | "direct" -> `Direct | _ -> `Crunch
let opt_string_of_env x = Some x
let string_of_env x = x

let get_env name fn =
  let res = Sys.getenv name in
  env_info "%s => %s" name res;
  fn (String.lowercase res)

let get_exn name fn =
  try get_env name fn
  with Not_found ->
    err "%s is not set." name

let get ~default name fn =
  try get_env name fn
  with Not_found ->
    env_info "%s unset => %s" name default;
    fn (String.lowercase default)

let fs = get "FS" ~default:"crunch" fs_of_env
let deploy = get "DEPLOY" ~default:"false" bool_of_env
let net = get "NET" ~default:"socket" net_of_env
let dhcp = get "DHCP" ~default:"false" bool_of_env

let blocks = ref 0
let mkfs fs path =
  let fat_of_files dir = kv_ro_of_fs (fat_of_files ~dir ()) in
  let fat_of_device device =
    let block = block_of_file (string_of_int device) in
    let fat   = fat block in
    kv_ro_of_fs fat
  in
  match fs, get_mode () with
  | `Fat   , `Xen -> incr blocks; fat_of_device (51711 + !blocks)
  | `Fat   , _    -> fat_of_files path
  | `Crunch, _    -> crunch path
  | `Direct, `Xen -> crunch path
  | `Direct, _    -> direct_kv_ro path

let cons0 = default_console

let stack = match deploy with
  | true ->
    let staticip =
      let address = get_exn "IP" Ipaddr.V4.of_string_exn in
      let netmask = get_exn "NETMASK" Ipaddr.V4.of_string_exn in
      let gateways = get_exn "GATEWAYS" ips_of_env in
      { address; netmask; gateways }
    in
    direct_stackv4_with_static_ipv4 cons0 tap0 staticip
  | false ->
    match net, dhcp with
    | `Direct, false -> direct_stackv4_with_default_ipv4 cons0 tap0
    | `Direct, true  -> direct_stackv4_with_dhcp cons0 tap0
    | `Socket, _     -> socket_stackv4 cons0 [Ipaddr.V4.any]
