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

open Ppxlib
open Ast_helper

let fail_with ppf =
  Format.kasprintf
    (fun message ->
      let location = !default_loc in
      let error = Location.Error.make ~loc:location message ~sub:[] in
      let extension = Location.Error.to_extension error in
      Exp.extension ~loc:location extension)
    ppf
;;

let ( ~: ) x = Lident x
let ( >> ) x y = Ldot (x, y)

let make_ident longident =
  let loc = !default_loc
  and txt = longident in
  Exp.ident { loc; txt }
;;

let application fun_ident parameters =
  Exp.apply
    (make_ident fun_ident)
    (List.map (fun parameter -> Asttypes.Nolabel, parameter) parameters)
;;

let expander_with_default_location transformation location str =
  with_default_loc location (fun () -> transformation str)
;;

let constant_rule char transformation =
  Context_free.Rule.constant
    Integer
    char
    (expander_with_default_location transformation)
;;
