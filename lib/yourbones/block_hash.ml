(*  MIT License

    Copyright (c) 2023 funkywork

    Permission is hereby granted, free of charge, to any person obtaining a copy
    of this software and associated documentation files (the "Software"), to deal
    in the Software without restriction, including without limitation the rights
    to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
    copies of the Software, and to permit persons to whom the Software is
    furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all
    copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
    IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
    FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
    AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
    LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
    OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
    SOFTWARE. *)

type t = string

let prefix = "\001\052"

type error =
  [ `Block_hash_invalid_length of string
  | `Block_hash_invalid_checksum of string
  ]

exception Block_hash_exception of error

let from_string str =
  let len = String.length str in
  if not (Int.equal len 51)
  then Error (`Block_hash_invalid_length str)
  else (
    let encoded = Tezos_base58.Base58 str in
    match Tezos_base58.decode ~prefix encoded with
    | None -> Error (`Block_hash_invalid_checksum str)
    | Some _ -> Ok str)
;;

let from_string' str =
  match from_string str with
  | Ok x -> x
  | Error err -> raise (Block_hash_exception err)
;;

let to_string str = str
let pp ppf hash = Format.fprintf ppf "%s" hash
let equal a b = String.equal a b

let pp_error ppf = function
  | `Block_hash_invalid_checksum s ->
    Format.fprintf ppf "`Block_hash_invalid_checksum (\"%s\")" s
  | `Block_hash_invalid_length s ->
    Format.fprintf ppf "`Block_hash_invalid_length (\"%s\")" s
;;

let equal_error a b =
  match a, b with
  | `Block_hash_invalid_checksum a, `Block_hash_invalid_checksum b ->
    String.equal a b
  | `Block_hash_invalid_length a, `Block_hash_invalid_length b ->
    String.equal a b
  | _ -> false
;;

let encoding =
  let open Data_encoding in
  conv to_string from_string' string
;;

module Fragment = struct
  type nonrec t = t

  let fragment_name = ":block_hash"
  let fragment_from_string repr = repr |> from_string |> Result.to_option
  let fragment_to_string x = to_string x
end

let fragment = Nightmare_service.Path.variable' (module Fragment)
