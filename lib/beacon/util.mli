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

(** some non-exported utility functions used to implement binding. *)

open Js_of_ocaml

(** {1 String util} *)

(** [normalize s] apply [trim] and [lowercase_ascii] on [s]. *)
val normalize : string -> string

(** {1 Conversion from and to JavaScript} *)

(** [array_to_js_with f arr] converts an OCaml Array ([arr]) into a javascript
    Array by applying a function [f] to each element. *)
val array_to_js_with : ('a -> 'b) -> 'a array -> 'b Js.js_array Js.t

(** [array_to_js arr] converts an OCaml Array ([arr]) into a javascript Array. *)
val array_to_js : 'a array -> 'a Js.js_array Js.t

(** [list_to_js_with f list] converts an OCaml List ([list]) into a javascript
    Array by applying a function [f] to each element. *)
val list_to_js_with : ('a -> 'b) -> 'a list -> 'b Js.js_array Js.t

(** [list_to_js list] converts an OCaml List ([list]) into a javascript Array. *)
val list_to_js : 'a list -> 'a Js.js_array Js.t

(** [array_from_js_with f arr] converts a javascript Array ([arr]) into an OCaml
    Array by applying a function [f] to each element. *)
val array_from_js_with : ('a -> 'b) -> 'a Js.js_array Js.t -> 'b array

(** [array_from_js arr] converts a javascript Array ([arr]) into an OCaml Array. *)
val array_from_js : 'a Js.js_array Js.t -> 'a array

(** [list_from_js_with f arr] converts a javascript Array ([arr]) into an OCaml
    List by applying a function [f] to each element. *)
val list_from_js_with : ('a -> 'b) -> 'a Js.js_array Js.t -> 'b list

(** [list_from_js arr] converts a javascript Array ([arr]) into an OCaml List. *)
val list_from_js : 'a Js.js_array Js.t -> 'a list

(** {1 Option util} *)

module List_option :
  Preface.Specs.TRAVERSABLE
    with type 'a t = 'a option
     and type 'a iter = 'a list
