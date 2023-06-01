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
open Nightmare_js

type t =
  { type_ : Yourbones.network_type
  ; name : string option
  ; rpc_url : string option
  }

let from_js network =
  let open Undefinable in
  let name = Js.to_string <$> network##.name |> to_option in
  let rpc_url = Js.to_string <$> network##.rpcUrl |> to_option in
  let type_ =
    let open Yourbones in
    network##._type
    |> Js.to_string
    |> Network.Type.from_string
    |> Option.value ~default:Network.Type.Custom
  in
  { type_; name; rpc_url }
;;

let to_js { type_; rpc_url; name } =
  let open Yourbones in
  let open Option in
  object%js
    val name = Js.string <$> name |> to_optdef
    val rpcUrl = Js.string <$> rpc_url |> to_optdef
    val _type = type_ |> Network.Type.to_string |> Js.string
  end
;;

let custom ~name ~rpc_url =
  let type_ = Yourbones.Network.Type.Custom in
  let name = Some name in
  let rpc_url = Some rpc_url in
  { type_; name; rpc_url }
;;

let mainnet ?name ?rpc_url () =
  let type_ = Yourbones.Network.Type.Mainnet in
  { type_; name; rpc_url }
;;

let mondaynet ?name ?rpc_url () =
  let type_ = Yourbones.Network.Type.Mondaynet in
  { type_; name; rpc_url }
;;

let dailynet ?name ?rpc_url () =
  let type_ = Yourbones.Network.Type.Dailynet in
  { type_; name; rpc_url }
;;

let ghostnet ?name ?rpc_url () =
  let type_ = Yourbones.Network.Type.Ghostnet in
  { type_; name; rpc_url }
;;

let nairobinet ?name ?rpc_url () =
  let type_ = Yourbones.Network.Type.Nairobinet in
  { type_; name; rpc_url }
;;
