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

(** A representation of a Tezos address. *)

(** {1 Types} *)

(** An address is a string encoded in base58..*)
type t

(** Errors related to Address. *)
type error =
  [ `Address_invalid_prefix of string
  | `Address_invalid_checksum of string
  | `Address_invalid_length of string
  ]

(** An exception used to lift [error] into an exception. *)
exception Address_exception of error

(** {1 API}*)

(** {2 COnstructing address from string} *)

(** [from_string addr] lift a string into an address, the address is wrapped
    into a result. *)
val from_string : string -> (t, [> error ]) result

val tz1 : string -> (t, [> error ]) result
val tz2 : string -> (t, [> error ]) result
val tz3 : string -> (t, [> error ]) result
val kt1 : string -> (t, [> error ]) result

(** an exception ful version of [from_string]. *)
val from_string' : string -> t

(** {2 Util} *)

val to_string : t -> string
val pp : Format.formatter -> t -> unit
val equal : t -> t -> bool
val pp_error : Format.formatter -> error -> unit
val equal_error : error -> error -> bool
val fragment : t Nightmare_service.Path.variable
