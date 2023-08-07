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

(** Generalization of signatures in the manner of the method described in this
    article:
    {{:https://www.craigfe.io/posts/generalised-signatures} Generalised
    signatures}. *)

(** {1 Numerical signatures}

    A set of signatures that deal with the processing of numerical numbers (e.g.
    [tez], or natural numbers). *)

module type TEZ = sig
  (** A common interface to manage the different representations of [tez]
      ([microtez] or [tez]). *)

  (** The type representing the numeric value (usually [Int64.t]). *)
  type t

  (** [one] is one amount of [t]. *)
  val one : t

  (** [from_int x] lift an [int] into a [t]. The function fails if [x] is
      negative. *)
  val from_int
    :  int
    -> ( t
       , [> `Tez_negative_amount of int64
         | `Tez_overflow
         | `Tez_invalid_multiplicator of int64
         ] )
       result

  (** [from_int x] lift an [int64] into a [t]. The function fails if [x] is
      negative. *)
  val from_int64
    :  int64
    -> ( t
       , [> `Tez_negative_amount of int64
         | `Tez_overflow
         | `Tez_invalid_multiplicator of int64
         ] )
       result

  (** [truncate x] drop the floating part of [x]. *)
  val truncate : t -> t

  (** [succ x] returns [t + one]. The function fails if [x] reach the overflow. *)
  val succ : t -> (t, [> `Tez_overflow ]) result

  (** [pred x] returns [t - one]. The function fails if [x] reach a negative
      amount. *)
  val pred : t -> (t, [> `Tez_negative_amount of int64 ]) result
end
