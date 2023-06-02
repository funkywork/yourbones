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

let from_mutez_repr value =
  let mutez = Tez.to_int64 value in
  let path = Util.(~:"Yourbones" >> "Tez" >> "from_mutez'") in
  let value = mutez |> Const.int64 |> Exp.constant in
  Util.application path [ value ]
;;

let fail_with_error ?location kind str err =
  Util.fail_with
    ?location
    {|"%s" projection into %s fails with %a|}
    str
    kind
    Tez.pp_error
    err
;;

let from_string kind f str =
  match Int64.of_string_opt str with
  | None -> Util.fail_with {|"%s" is not a valid %s representation|} str kind
  | Some value ->
    Result.fold ~ok:from_mutez_repr ~error:(fail_with_error kind str) (f value)
;;

let mutez = from_string "mutez" Tez.Micro.from_int64
let tez = from_string "tez" Tez.from_int64

let register =
  ( "yourbones_ppx.tez_literal"
  , Util.[ constant_rule 'm' mutez; constant_rule 't' tez ] )
;;
