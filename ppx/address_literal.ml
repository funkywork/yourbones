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

let extracter () = Ast_pattern.(single_expr_payload (estring __))

let fail_with_error ?location kind str err =
  Util.fail_with
    ?location
    {|"%s" projection into a "%s" address fails with %a|}
    str
    kind
    Address.pp_error
    err
;;

let generic_expander (k_str, f) ~ctxt potential_address =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  match f potential_address with
  | Error err -> fail_with_error ~location:loc k_str potential_address err
  | Ok _ ->
    let path = Util.(~:"Yourbones" >> "Address" >> k_str) in
    let expr =
      Util.application
        path
        [ Ast_builder.Default.estring ~loc potential_address ]
    in
    [%expr Result.get_ok [%e expr]]
;;

let tz1_extender =
  let ((extender_name, _) as kind) = "tz1", Address.tz1 in
  Extension.V3.declare
    extender_name
    Extension.Context.expression
    (extracter ())
    (generic_expander kind)
;;

let tz2_extender =
  let ((extender_name, _) as kind) = "tz2", Address.tz2 in
  Extension.V3.declare
    extender_name
    Extension.Context.expression
    (extracter ())
    (generic_expander kind)
;;

let tz3_extender =
  let ((extender_name, _) as kind) = "tz3", Address.tz3 in
  Extension.V3.declare
    extender_name
    Extension.Context.expression
    (extracter ())
    (generic_expander kind)
;;

let tz4_extender =
  let ((extender_name, _) as kind) = "tz4", Address.tz4 in
  Extension.V3.declare
    extender_name
    Extension.Context.expression
    (extracter ())
    (generic_expander kind)
;;

let kt1_extender =
  let kind = "kt1", Address.kt1 in
  Extension.V3.declare
    "KT1"
    Extension.Context.expression
    (extracter ())
    (generic_expander kind)
;;

let sr1_extender =
  let ((extender_name, _) as kind) = "sr1", Address.sr1 in
  Extension.V3.declare
    extender_name
    Extension.Context.expression
    (extracter ())
    (generic_expander kind)
;;

let register =
  ( "yourbones_ppx.address_literal"
  , [ Context_free.Rule.extension tz1_extender
    ; Context_free.Rule.extension tz2_extender
    ; Context_free.Rule.extension tz3_extender
    ; Context_free.Rule.extension tz4_extender
    ; Context_free.Rule.extension kt1_extender
    ; Context_free.Rule.extension sr1_extender
    ] )
;;
