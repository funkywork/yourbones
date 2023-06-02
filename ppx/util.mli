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

(** Generate a compile-time error*)
val fail_with
  :  ?location:Ppxlib.location
  -> ('a, Format.formatter, unit, Ppxlib.expression) format4
  -> 'a

(** Building module path using Infix operators.contents The usual way is to do
    that: [~:"Mod1" >> "Mod2" >> "foo"] to define the path [Mod1.Mod2.foo]. *)

val ( ~: ) : string -> Ppxlib.longident
val ( >> ) : Ppxlib.longident -> string -> Ppxlib.longident

(** Converts a [longident] into an [expression]. *)
val make_ident : Ppxlib.longident -> Ppxlib.expression

(** [application f_ident parameters] will produce a function application of
    [f_ident] with the list of given parameters. *)
val application
  :  Ppxlib.longident
  -> Ppxlib.expression list
  -> Ppxlib.expression

(** Build an expander with the default location. *)
val expander_with_default_location : ('a -> 'b) -> Ppxlib.location -> 'a -> 'b

(** Build a constant rule expander (suffix in literal values) using default
    location (relay on [expander_with_default_location]). *)
val constant_rule
  :  char
  -> (string -> Ppxlib.expression)
  -> Ppxlib.Context_free.Rule.t
