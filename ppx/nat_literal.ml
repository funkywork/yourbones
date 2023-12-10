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

open Yourbones
open Ppxlib
open Ast_helper

let from_nat_repr value =
  let nat = Nat.to_int64 value in
  let path = Util.(~:"Yourbones" >> "Nat" >> "abs_64") in
  let value = nat |> Const.int64 |> Exp.constant in
  Util.application path [ value ]
;;

let fail_with_error ?location str =
  Util.fail_with ?location {|"%s" projection into Natural impossible|} str
;;

let from_string str =
  match Nat.from_string str with
  | None -> fail_with_error str
  | Some value -> from_nat_repr value
;;

let register =
  "Yourbones_ppx.nat_literal", Util.[ constant_rule 'N' from_string ]
;;
