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

(** Represents a natural number (which can never be negative). *)

(** {1 Types} *)

(** A type that describes a [nat] number. *)
type t

(** {1 Create Nat values} *)

(** [from_int x] lift [x] in [nat] if the number is greater or equal to zero. *)
val from_int : int -> t option

(** [from_int64 x] lift [x] in [nat] if the number is greater or equal to
    zero. *)
val from_int64 : int64 -> t option

(** [from_z x] lift [x] in [nat] if the number is greater or equal to zero. *)
val from_z : Z.t -> t option

(** [from_string] is like [int_of_string_opt] but for [nat], taking care of the
    negative overflow. *)
val from_string : string -> t option

(** {2 Using absolute value}

    Always succeed the conversion using the absolute representation of the given
    value. *)

(** [abs x] lift [x] as a [nat] using absolute value. *)
val abs : int -> t

(** [abs_64 x] lift [x] as a [nat] using absolute value. *)
val abs_64 : int64 -> t

(** [abs_z x] lift [x] as a [nat] using absolute value. *)
val abs_z : Z.t -> t

(** [abs_string x] is like [x Int64.of_string_opt |> abs_64]. *)
val abs_string : string -> t option

(** {1 Conversion} *)

(** [to_int x] converts a [nat] into an [int]. *)
val to_int : t -> int

(** [to_int64 x] converts a [nat] into an [int64]. *)
val to_int64 : t -> int64

(** [to_z x] converts a [nat] into an [Z.t]. *)
val to_z : t -> Z.t

(** [to_string] converts a [nat] into a [string]. *)
val to_string : t -> string

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

(** The zero value of a [nat]. *)
val zero : t

(** The one value of a [nat]. *)
val one : t

(** [succ x] returns the successor of [x]. *)
val succ : t -> t

(** [pred x] returns the predecessor of [x].

    {b warning}: the function returns a [Z.t] since a substraction can lead to a
    negative number, so the conversion to [nat] is delayed at the user level. *)
val pred : t -> Z.t

(** [bounded_pred x] returns a bounded predecessor. ie: [bounded_pred zero] is
    zero. *)
val bounded_pred : t -> t

(** [add x y] addition between two [nats]. *)
val add : t -> t -> t

(** [sub x y] substraction  between two [nats].

    {b warning}: the function returns a [Z.t] since a substraction can lead to a
    negative number, so the conversion to [nat] is delayed at the user level. *)
val sub : t -> t -> Z.t

(** [bounded_sub x y] is like {!val:sub} but if the substraction reach a
    negative overflow, the result is bounded to [zero]. *)
val bounded_sub : t -> t -> t

(** [mul x y] multiplication between two [nats]. *)
val mul : t -> t -> t

(** [ediv x y] euclidian division between two [nats]. *)
val ediv : t -> t -> t option

(** [rem x y] euclidian division remainder between two [nats]. *)
val rem : t -> t -> t option

(** [ediv x y] euclidean division between two [nat] with the [remainer]. *)
val ediv_rem : t -> t -> (t * t) option

(** {1 Pretty printers} *)

val pp : Format.formatter -> t -> unit

(** {1 Infix operators} *)

module Infix : sig
  (** {2 Comparisons and equality} *)

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

  (** {2 Arithmetic} *)

  (** Infix version of {!val:add}. *)
  val ( + ) : t -> t -> t

  (** Infix version of {!val:sub}. *)
  val ( - ) : t -> t -> Z.t

  (** Infix version of {!val:bounded_sub}. *)
  val ( -^ ) : t -> t -> t

  (** Infix version of {!val:mul}. *)
  val ( * ) : t -> t -> t
end

include module type of Infix (** @inline *)

(** {1 Encoding} *)

val encoding : t Data_encoding.t
