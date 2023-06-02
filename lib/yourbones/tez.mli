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

(** The representation of the Tezos coin. A [tez] can never be negative, so some
    arithmetic operations may fail.

    A [tez] is actually expressed in Nanotez to avoid operations with floating
    numbers and can be represented as Nanotez or Microtez (mutez). *)

(** {1 Types} *)

(** A type that describes a [tez], expressed in [nanotez].*)
type t

(** Errors related to Tez. *)
type error =
  [ `Tez_negative_amount of int64
  | `Tez_overflow
  | `Tez_invalid_string_representation of string
  | `Tez_invalid_multiplicator of int64
  | `Tez_invalid_divisor of int64
  ]

(** An exception used to lift [tez_error] into an exception. *)
exception Tez_exception of error

(** {1 Misc} *)

(** returns the "ꜩ" caracter. *)
val symbol : string

(** {1 Floating part} *)

module Micro : sig
  include Interfaces.TEZ with type t := t (** @inline *)
end

(** {1 Manipulating Tez} *)

(** [zero] is [0tez]. *)
val zero : t

(** Pretty-printer. *)
val pp
  :  ?floating_part:[ `None | `One | `Two | `Three | `Four | `Five | `Six ]
  -> unit
  -> Format.formatter
  -> t
  -> unit

(** Pretty-printer with suffix *)
val pp_print : Format.formatter -> t -> unit

(** [to_mutez x] returns the result as [int64] in [mutez]. *)
val to_mutez : t -> int64

(** [from_mutez x] alias of {!val:Micro.from_int64}. *)
val from_mutez
  :  int64
  -> ( t
     , [> `Tez_negative_amount of int64
       | `Tez_overflow
       | `Tez_invalid_multiplicator of int64
       ] )
     result

(** [from_mutez' x] is an exceptionful version of [from_mutez]. *)
val from_mutez' : int64 -> t

(** [to_int64 x] cast [x] into an [int64]. *)
val to_int64 : t -> int64

include Interfaces.TEZ with type t := t (** @inline *)

val from_string
  :  string
  -> (t, [> `Tez_invalid_string_representation of string ]) result

(** {2 Comparison and predicate} *)

(** [compare x y] returns [0] if [x] equal [y], a negative integer if [x] is
    less than [y] and a positive integer if [x] is greater than [y]. *)
val compare : t -> t -> int

(** [equal x y] returns [true] if [x] and [y] are equal, [false] otherwise. *)
val equal : t -> t -> bool

(** {2 Ordering} *)

(** [min x y] returns the smallest of [x] and [y]. *)
val min : t -> t -> t

(** [max x y] returns the greatest of [x] and [y]. *)
val max : t -> t -> t

(** {2 Arithmetic} *)

(** [add x y] is the sum between [x] and [y]. The function fails if the sum
    reach the overflow. *)
val add : t -> t -> (t, [> `Tez_overflow ]) result

(** [sub x y] is the substraction between [x] and [y]. The function fails if the
    substraction reach a negative amount. *)
val sub : t -> t -> (t, [> `Tez_negative_amount of int64 ]) result

(** [mul x y] is the multiplication of [x] by [y]. The function fails if the [y]
    is negative or if the result reach the overflow.*)
val mul
  :  t
  -> int64
  -> (t, [> `Tez_overflow | `Tez_invalid_multiplicator of int64 ]) result

(** [div x y] is the division of [x] by [y]. The function fails if the [y] is
    negative or zero.*)
val div : t -> int64 -> (t, [> `Tez_invalid_divisor of int64 ]) result

(** {2 Infix operators} *)

module Infix : sig
  (** {3 Comparisons and equality} *)

  (** Infix version of {!val:equal}. *)
  val ( = ) : t -> t -> bool

  (** [x <> y] is [not x = y]. *)
  val ( <> ) : t -> t -> bool

  (** [x > y] returns [true] if [x] is strictly greater than [y]. *)
  val ( > ) : t -> t -> bool

  (** [x > y] returns [true] if [x] is greater or equal to [y]. *)
  val ( >= ) : t -> t -> bool

  (** [x > y] returns [true] if [y] is strictly greater than [x]. *)
  val ( < ) : t -> t -> bool

  (** [x > y] returns [true] if [y] is greater or equal to [x]. *)
  val ( <= ) : t -> t -> bool

  (** {3 Arithmetic} *)

  (** Infix version of {!val:add}. *)
  val ( + ) : t -> t -> (t, [> `Tez_overflow ]) result

  (** Infix version of {!val:sub}. *)
  val ( - ) : t -> t -> (t, [> `Tez_negative_amount of int64 ]) result

  (** Infix version of {!val:mul}. *)
  val ( * )
    :  t
    -> int64
    -> (t, [> `Tez_overflow | `Tez_invalid_multiplicator of int64 ]) result

  (** Infix version of {!val:div}. *)
  val ( / ) : t -> int64 -> (t, [> `Tez_invalid_divisor of int64 ]) result

  (** {3 Binded arithmetic}

      As many tez functions can fail, here are some conveinent operators to
      treat them as normal arithmetic operations. The scheme is pretty simple
      (where [+] can be change by any other arithmetic operator) :

      - [x |+ y] is [x + y] where [x] is wrapped into a result
      - [x +| y] is [x + y] where [y] is wrapped into a result
      - [x |+| y] is [x + y] where [x] and [y] are both wrapped into a result.

      This makes the definition of complex (and potentially failing) arithmetic
      expressions relatively trivial!

      {[
        let value =
          my_balance +| succ one
          |+| from_int 45
          |-| transaction -| from_int64 12L
        ;;
      ]}

      However, it is likely that such operations are rarely useful... *)

  (** [x |+ y] is [x + y] where [x] is wrapped into a result. *)
  val ( |+ ) : (t, (error as 'err)) result -> t -> (t, 'err) result

  (** [x +| y] is [x + y] where [y] is wrapped into a result. *)
  val ( +| ) : t -> (t, (error as 'err)) result -> (t, 'err) result

  (** [x |+| y] is [x + y] where [x] and [y] are wrapped into a result. *)
  val ( |+| )
    :  (t, (error as 'err)) result
    -> (t, 'err) result
    -> (t, 'err) result

  (** [x |- y] is [x - y] where [x] is wrapped into a result. *)
  val ( |- ) : (t, (error as 'err)) result -> t -> (t, 'err) result

  (** [x -| y] is [x - y] where [y] is wrapped into a result. *)
  val ( -| ) : t -> (t, (error as 'err)) result -> (t, 'err) result

  (** [x |-| y] is [x - y] where [x] and [y] are wrapped into a result. *)
  val ( |-| )
    :  (t, (error as 'err)) result
    -> (t, 'err) result
    -> (t, 'err) result

  (** [x |* y] is [x * y] where [x] is wrapped into a result. *)
  val ( |* ) : (t, (error as 'err)) result -> int64 -> (t, 'err) result

  (** [x *| y] is [x * y] where [y] is wrapped into a result. *)
  val ( *| ) : int64 -> (t, (error as 'err)) result -> (t, 'err) result

  (** [x |/ y] is [x / y] where [x] is wrapped into a result. *)
  val ( |/ ) : (t, (error as 'err)) result -> t -> (t, 'err) result
end

include module type of Infix (** @inline *)

(** {1 Error handling} *)

(** Pretty printer for errors. *)
val pp_error : Format.formatter -> error -> unit

(** Equality between errors. *)
val equal_error : error -> error -> bool
