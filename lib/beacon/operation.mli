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

(** Describes possible operations on Tezos (in a JS Fashion) *)
(* FIXME: relay on Tezos instead of Beacon *)

open Js_of_ocaml

module Kind : sig
  type t = Transaction

  val to_string : t -> string
  val from_string : string -> t option
end

module Partial : sig
  type t =
    { kind : Kind.t
    ; source : Yourbones.Address.t option
    ; fee : Yourbones.Tez.t option
    ; counter : Z.t option
    ; gas_limit : Z.t option
    ; storage_limit : Z.t option
    }

  val to_js : t -> Bindings.partial_tezos_operation Js.t
end

module Transaction : sig
  (* FIXME: Handler parameters. *)
  type t =
    { source : Yourbones.Address.t option
    ; fee : Yourbones.Tez.t option
    ; counter : Z.t option
    ; gas_limit : Z.t option
    ; storage_limit : Z.t option
    ; amount : Yourbones.Tez.t
    ; destination : Yourbones.Address.t
    }

  val forge
    :  ?source:Yourbones.Address.t
    -> ?fee:Yourbones.tez
    -> ?counter:Z.t
    -> ?gas_limit:Z.t
    -> ?storage_limit:Z.t
    -> destination:Yourbones.Address.t
    -> Yourbones.tez
    -> t

  val to_js : t -> Bindings.tezos_transaction Js.t
  val pack : t -> Bindings.packed_tezos_operation Js.t
end
