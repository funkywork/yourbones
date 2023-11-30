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

(** A very simple implementation of Micheline, a kind of S-Expression, vendored for
    OCaml compatibility reasons. *)

(** {1 AST} *)

(** the AST of a micheline expression parameterised by a location and a primitive
    type. *)
type ('location, 'primitive) t =
  | Int of 'location * Z.t
  | String of 'location * string
  | Bytes of 'location * bytes
  | Prim of 'location * 'primitive * ('location, 'primitive) t list * annotation
  | Seq of 'location * ('location, 'primitive) t list

(** Annotation attached to a primitive. *)
and annotation = string list

(** {2 Building AST components} *)

(** [z ~location x] constructs an [Int] node. *)
val z : location:'location -> Z.t -> ('location, 'primitive) t

(** [int ~location x] constructs an [Int] node. *)
val int : location:'location -> int -> ('location, 'primitive) t

(** [int64 ~location x] constructs an [Int] node. *)
val int64 : location:'location -> int64 -> ('location, 'primitive) t

(** [string ~location x] constructs a [String] node. *)
val string : location:'location -> string -> ('location, 'primitive) t

(** [bytes ~location x] constructs a [Bytes] node. *)
val bytes : location:'location -> bytes -> ('location, 'primitive) t

(** [prim ~location x xs] constructs a [Prim] node. *)
val prim
  :  ?annotation:annotation
  -> location:'location
  -> 'primitive
  -> ('location, 'primitive) t list
  -> ('location, 'primitive) t

(** [seq ~location xs] constructs a [Seq] node. *)
val seq
  :  location:'location
  -> ('location, 'primitive) t list
  -> ('location, 'primitive) t

(** {2 Extract data from AST} *)

(** [annotation micheline] returns the annotation attached to a node. *)
val annotation : (_, _) t -> annotation

(** [location micheline] returns the location attached to a node. *)
val location : ('location, _) t -> 'location

(** {2 Helpers} *)

(** Pretty printer for Micheline expression. *)
val pp
  :  ?pp_location:(Format.formatter -> 'location -> unit)
  -> pp_primitive:(Format.formatter -> 'primitive -> unit)
  -> unit
  -> Format.formatter
  -> ('location, 'primitive) t
  -> unit

(** Equality between two micheline expression. *)
val equal
  :  ('location -> 'location -> bool)
  -> ('primitive -> 'primitive -> bool)
  -> ('location, 'primitive) t
  -> ('location, 'primitive) t
  -> bool

(** {1 Canonical representation of micheline}

    A canonical representation of micheline expression uses integers as location trackers
    (where the root is node 0). *)

module Canonical : sig
  (** A reference of the previous type [t]. *)
  type ('location, 'primitive) node := ('location, 'primitive) t

  (** In a canonical expression, the location is encoded as an integer. *)
  type location = int

  (** A Micheline expression where the location is an integer. The type is abstract to
      guarantee the construction of canonical expressions with a correctly calculated
      location. *)
  type 'primitive t

  (** [from_node node] rebuild the AST with proper canonical location. *)
  val from_node : (_, 'primitive) node -> 'primitive t

  (** [to_node canonical] converts a canonical expression to a regular one using
      integer as location. *)
  val to_node : 'primitive t -> (location, 'primitive) node

  (** {2 Building AST components}

      Even if the function-based construction of the AST does not produce canonical
      expressions, it does not force you to pass a location because it will be the
      role of the [from_node] function to calculate the location. Ie:

      {[
        seq [ int 10; string "Hello" ] |> from_node
      ]} *)

  (** [z x] constructs an [Int] node. *)
  val z : Z.t -> (location, 'primitive) node

  (** [int  x] constructs an [Int] node. *)
  val int : int -> (location, 'primitive) node

  (** [int64 x] constructs an [Int] node. *)
  val int64 : int64 -> (location, 'primitive) node

  (** [string x] constructs a [String] node. *)
  val string : string -> (location, 'primitive) node

  (** [bytes x] constructs a [Bytes] node. *)
  val bytes : bytes -> (location, 'primitive) node

  (** [prim x xs] constructs a [Prim] node. *)
  val prim
    :  ?annotation:annotation
    -> 'primitive
    -> (location, 'primitive) node list
    -> (location, 'primitive) node

  (** [seq xs] constructs a [Seq] node. *)
  val seq : (location, 'primitive) node list -> (location, 'primitive) node

  (** {2 Extract data from AST} *)

  (** [annotation canonical] returns the annotation attached to a canonical node. *)
  val annotation : 'primitive t -> annotation

  (** [location canonical] returns the location attached to a canonical node. *)
  val location : 'primitive t -> location

  (** {2 Helpers} *)

  (** Equality between canonical micheline expression. *)
  val equal
    :  ('primitive -> 'primitive -> bool)
    -> 'primitive t
    -> 'primitive t
    -> bool
end

(** {1 Encoding} *)

(** [Data_encoding] for Micheline expression using an integer as location. *)
val encoding : 'primitive Data_encoding.t -> (int, 'primitive) t Data_encoding.t

(** [Data_encoding] for Canonical Micheline Expression. *)
val canonical_encoding
  :  'primitive Data_encoding.t
  -> 'primitive Canonical.t Data_encoding.t
