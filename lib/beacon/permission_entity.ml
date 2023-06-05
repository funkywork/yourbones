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

open Js_of_ocaml

type t =
  { address : Yourbones.Address.t
  ; network : Network.t
  ; scopes : Permission_scope.t list
  ; threshold : Threshold.t option
  }

let from_js permission_entity =
  let open Nightmare_js.Undefinable in
  let address =
    Js.to_string permission_entity##.address |> Yourbones.Address.from_string'
  in
  let network = Network.from_js permission_entity##.network in
  let scopes = Permission_scope.from_js_array permission_entity##.scopes in
  let threshold =
    Threshold.from_js <$> permission_entity##.threshold |> to_option
  in
  { address; network; scopes; threshold }
;;

let to_js { address; network; scopes; threshold } =
  let open Preface.Fun.Infix in
  let open Nightmare_js.Option in
  object%js
    val address = Js.string (address |> Yourbones.Address.to_string)
    val network = Network.to_js network

    val scopes =
      Util.list_to_js_with (Js.string % Permission_scope.to_string) scopes

    val threshold = Threshold.to_js <$> threshold |> to_optdef
  end
;;
