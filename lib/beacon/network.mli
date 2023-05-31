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

(** Describes a network by type, potential name and potential rpc server
    address.*)

open Js_of_ocaml

type t =
  { type_ : Yourbones_common.network_type
  ; name : string option
  ; rpc_url : string option
  }

val from_js : Bindings.network Js.t -> t
val to_js : t -> Bindings.network Js.t

(** {1 Networks list} *)

val custom : name:string -> rpc_url:string -> t
val mainnet : ?name:string -> ?rpc_url:string -> unit -> t
val mondaynet : ?name:string -> ?rpc_url:string -> unit -> t
val dailynet : ?name:string -> ?rpc_url:string -> unit -> t
val ghostnet : ?name:string -> ?rpc_url:string -> unit -> t
val nairobinet : ?name:string -> ?rpc_url:string -> unit -> t
