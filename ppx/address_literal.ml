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

let generic_expander k_str f ~ctxt potential_address =
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

let address_expander =
  Extension.V3.declare
    "address"
    Extension.Context.expression
    (extracter ())
    (generic_expander "from_string" Address.from_string)
;;

let tz1_extender =
  Extension.V3.declare
    "tz1"
    Extension.Context.expression
    (extracter ())
    (generic_expander "tz1" Address.tz1)
;;

let tz2_extender =
  Extension.V3.declare
    "tz2"
    Extension.Context.expression
    (extracter ())
    (generic_expander "tz2" Address.tz2)
;;

let tz3_extender =
  Extension.V3.declare
    "tz3"
    Extension.Context.expression
    (extracter ())
    (generic_expander "tz3" Address.tz3)
;;

let kt1_extender =
  Extension.V3.declare
    "kt1"
    Extension.Context.expression
    (extracter ())
    (generic_expander "kt1" Address.kt1)
;;

let register =
  ( "yourbones_ppx.address_literal"
  , [ Context_free.Rule.extension tz1_extender
    ; Context_free.Rule.extension tz2_extender
    ; Context_free.Rule.extension tz3_extender
    ; Context_free.Rule.extension kt1_extender
    ; Context_free.Rule.extension address_expander
    ] )
;;
