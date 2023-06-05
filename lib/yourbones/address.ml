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

type t =
  | Tz1 of string
  | Tz2 of string
  | Tz3 of string
  | Kt1 of string

type error =
  [ `Address_invalid_prefix of string
  | `Address_invalid_checksum of string
  | `Address_invalid_length of string
  ]

exception Address_exception of error

let ed25519_prefix = "\006\161\159"
let secp256k1_prefix = "\006\161\161"
let p256_prefix = "\006\161\164"
let nocurve_prefix = "\002\090\121"

let from_string str =
  let len = String.length str in
  if not (Int.equal len 36)
  then Error (`Address_invalid_length str)
  else (
    let prefix_with_fun =
      match str.[0], str.[1], str.[2] with
      | 't', 'z', '1' -> Some ((fun x -> Tz1 x), ed25519_prefix)
      | 't', 'z', '2' -> Some ((fun x -> Tz2 x), secp256k1_prefix)
      | 't', 'z', '3' -> Some ((fun x -> Tz3 x), p256_prefix)
      | 'K', 'T', '1' -> Some ((fun x -> Kt1 x), nocurve_prefix)
      | _ -> None
    in
    match prefix_with_fun with
    | None -> Error (`Address_invalid_prefix str)
    | Some (construct, prefix) ->
      let encoded = Tezos_base58.Base58 str in
      (match Tezos_base58.decode ~prefix encoded with
       | None -> Error (`Address_invalid_checksum str)
       | Some decoded ->
         if Int.equal (String.length decoded) 20
         then Ok (construct str)
         else Error (`Address_invalid_length str)))
;;

let tz1 str =
  match from_string str with
  | Ok (Tz1 x) -> Ok (Tz1 x)
  | Ok _ -> Error (`Address_invalid_prefix str)
  | Error err -> Error err
;;

let tz2 str =
  match from_string str with
  | Ok (Tz2 x) -> Ok (Tz2 x)
  | Ok _ -> Error (`Address_invalid_prefix str)
  | Error err -> Error err
;;

let tz3 str =
  match from_string str with
  | Ok (Tz3 x) -> Ok (Tz3 x)
  | Ok _ -> Error (`Address_invalid_prefix str)
  | Error err -> Error err
;;

let kt1 str =
  match from_string str with
  | Ok (Kt1 x) -> Ok (Kt1 x)
  | Ok _ -> Error (`Address_invalid_prefix str)
  | Error err -> Error err
;;

let from_string' str =
  match from_string str with
  | Ok x -> x
  | Error err -> raise (Address_exception err)
;;

let pp ppf = function
  | Tz1 x | Tz2 x | Tz3 x | Kt1 x -> Format.fprintf ppf "%s" x
;;

let equal a b =
  match a, b with
  | Tz1 a, Tz1 b | Tz2 a, Tz2 b | Tz3 a, Tz3 b | Kt1 a, Kt1 b ->
    String.equal a b
  | _ -> false
;;

let pp_error ppf = function
  | `Address_invalid_prefix str ->
    Format.fprintf ppf "`Address_invalid_prefix (\"%s\")" str
  | `Address_invalid_checksum str ->
    Format.fprintf ppf "`Address_invalid_checksum (\"%s\")" str
  | `Address_invalid_length str ->
    Format.fprintf ppf "`Address_invalid_length (\"%s\")" str
;;

let equal_error a b =
  match a, b with
  | `Address_invalid_prefix a, `Address_invalid_prefix b -> String.equal a b
  | `Address_invalid_checksum a, `Address_invalid_checksum b -> String.equal a b
  | `Address_invalid_length a, `Address_invalid_length b -> String.equal a b
  | _ -> false
;;

let to_string = function
  | Tz1 x | Tz2 x | Tz3 x | Kt1 x -> x
;;
