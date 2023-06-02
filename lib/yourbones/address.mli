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

(** A representation of a Tezos address. For the moment, address type indexing
    only serves to define which keys are supported by the library.*)

(** {1 Supported address types} *)

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

(** {1 Types} *)

type 'a t constraint 'a = kind

type error =
  [ `Address_invalid_prefix of string
  | `Address_invalid_checksum of string
  | `Address_invalid_length of string
  ]

(** {1 API}*)

(** {2 COnstructing address from string} *)

val tz1 : string -> ([> tz1 ] t, [> error ]) result
val tz2 : string -> ([> tz2 ] t, [> error ]) result
val tz3 : string -> ([> tz3 ] t, [> error ]) result
val tz4 : string -> ([> tz4 ] t, [> error ]) result
val kt1 : string -> ([> kt1 ] t, [> error ]) result
val sr1 : string -> ([> sr1 ] t, [> error ]) result
val from_string : string -> ('a t, [> error ]) result

(** {2 Util} *)

val to_string : _ t -> string
val pp : Format.formatter -> _ t -> unit
val equal : _ t -> _ t -> bool
val pp_error : Format.formatter -> error -> unit
val equal_error : error -> error -> bool
