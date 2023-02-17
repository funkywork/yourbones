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

let fail_with ppf =
  Format.kasprintf
    (fun message ->
      let open Ast_helper in
      let location = !default_loc in
      let error = Location.Error.make ~loc:location message ~sub:[] in
      let extension = Location.Error.to_extension error in
      Exp.extension ~loc:location extension)
    ppf
;;

let make_longident lid =
  let open Ast_helper in
  let loc = !default_loc in
  let txt =
    let open Longident in
    Ldot (Ldot (Lident "Yourbones_common", "Tez"), lid)
  in
  Exp.ident { loc; txt }
;;

let fun_apply fun_name parameters =
  let open Ast_helper in
  let open Exp in
  apply
    (make_longident fun_name)
    (List.map (fun p -> Asttypes.Nolabel, p) parameters)
;;

let microtez str =
  match Int64.of_string_opt str with
  | None -> fail_with {|"%s" is not a valid microtez representation|} str
  | Some value ->
    (match Yourbones_common.Tez.Micro.from_int64 value with
     | Ok x ->
       let concrete_value = Yourbones_common.Tez.to_int64 x in
       fun_apply
         "from_mutez'"
         Ast_helper.[ Exp.constant (Const.int64 concrete_value) ]
     | Error err ->
       fail_with
         {|"%s" projection into microtez fail with %a|}
         str
         Yourbones_common.Tez.pp_error
         err)
;;

let tez str =
  match Int64.of_string_opt str with
  | None -> fail_with {|"%s" is not a valid tez representation|} str
  | Some value ->
    (match Yourbones_common.Tez.from_int64 value with
     | Ok x ->
       let concrete_value = Yourbones_common.Tez.to_int64 x in
       fun_apply
         "from_mutez'"
         Ast_helper.[ Exp.constant (Const.int64 concrete_value) ]
     | Error err ->
       fail_with
         {|"%s" projection into tez fail with %a|}
         str
         Yourbones_common.Tez.pp_error
         err)
;;

let expander transformation location str =
  Ast_helper.with_default_loc location (fun () -> transformation str)
;;

let rule char transformation =
  Context_free.Rule.constant Integer char (expander transformation)
;;

let () =
  Driver.register_transformation
    "yourbones_ppx"
    ~rules:[ rule 'm' microtez; rule 't' tez ]
;;
