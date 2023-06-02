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

type kind =
  [ `Ed25519
  | `Secp256k1
  | `P256
  | `NoCurve
  | `Bls12_381
  | `SR1
  ]

type tz1 = [ `Ed25519 ]
type tz2 = [ `Secp256k1 ]
type tz3 = [ `P256 ]
type tz4 = [ `Bls12_381 ]
type kt1 = [ `NoCurve ]
type sr1 = [ `SR1 ]
type 'a t = 'a * string constraint 'a = kind

type error =
  [ `Address_invalid_prefix of string
  | `Address_invalid_checksum of string
  | `Address_invalid_length of string
  ]

let ed25519_prefix = "\006\161\159"
let secp256k1_prefix = "\006\161\161"
let p256_prefix = "\006\161\164"
let bls12_381_prefix = "\006\161\166"
let nocurve_prefix = "\002\090\121"
let sr1_prefix = "\006\124\117"

let from_string str =
  match
    if String.starts_with ~prefix:"tz1" str
    then Ok (`Ed25519, ed25519_prefix)
    else if String.starts_with ~prefix:"tz2" str
    then Ok (`Secp256k1, secp256k1_prefix)
    else if String.starts_with ~prefix:"tz3" str
    then Ok (`P256, p256_prefix)
    else if String.starts_with ~prefix:"tz4" str
    then Ok (`Bls12_381, bls12_381_prefix)
    else if String.starts_with ~prefix:"KT1" str
    then Ok (`NoCurve, nocurve_prefix)
    else if String.starts_with ~prefix:"sr1" str
    then Ok (`SR1, sr1_prefix)
    else Error (`Address_invalid_prefix str)
  with
  | Ok (kind, prefix) ->
    let encoded = Tezos_base58.Base58 str in
    (match Tezos_base58.decode ~prefix encoded with
     | Some decoded ->
       if Int.equal (String.length decoded) 20
       then Ok (kind, str)
       else
         (* Hard to provides a counter-example *)
         Error (`Address_invalid_length str)
     | None -> Error (`Address_invalid_checksum str))
  | Error err -> Error err
;;

let tz1 str =
  match from_string str with
  | Ok ((`Ed25519, _) as str) -> Ok str
  | Ok _ -> Error (`Address_invalid_prefix str)
  | Error err -> Error err
;;

let tz2 str =
  match from_string str with
  | Ok ((`Secp256k1, _) as str) -> Ok str
  | Ok _ -> Error (`Address_invalid_prefix str)
  | Error err -> Error err
;;

let tz3 str =
  match from_string str with
  | Ok ((`P256, _) as str) -> Ok str
  | Ok _ -> Error (`Address_invalid_prefix str)
  | Error err -> Error err
;;

let tz4 str =
  match from_string str with
  | Ok ((`Bls12_381, _) as str) -> Ok str
  | Ok _ -> Error (`Address_invalid_prefix str)
  | Error err -> Error err
;;

let kt1 str =
  match from_string str with
  | Ok ((`NoCurve, _) as str) -> Ok str
  | Ok _ -> Error (`Address_invalid_prefix str)
  | Error err -> Error err
;;

let sr1 str =
  match from_string str with
  | Ok ((`SR1, _) as str) -> Ok str
  | Ok _ -> Error (`Address_invalid_prefix str)
  | Error err -> Error err
;;

let pp ppf (_, x) = Format.fprintf ppf "%s" x
let equal (_, a) (_, b) = String.equal a b

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

let to_string (_, x) = x
