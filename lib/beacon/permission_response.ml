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
  { version : string
  ; id : string
  ; sender_id : string
  ; app_metadata : App_metadata.t
  ; public_key : string
  ; network : Network.t
  ; scopes : Permission_scope.t list
  ; threshold : Threshold.t option
  ; notification : Notification.t option
  }

let from_js response =
  let Beacon_base_message.{ version; id; sender_id } =
    Beacon_base_message.from_js response
  in
  let open Nightmare_js.Undefinable in
  let app_metadata = App_metadata.from_js response##.appMetadata in
  let public_key = Js.to_string response##.publicKey in
  let network = Network.from_js response##.network in
  let scopes = Permission_scope.from_js_array response##.scopes in
  let threshold = Threshold.from_js <$> response##.threshold |> to_option in
  let notification =
    Notification.from_js <$> response##.notification |> to_option
  in
  { version
  ; id
  ; sender_id
  ; app_metadata
  ; public_key
  ; network
  ; scopes
  ; threshold
  ; notification
  }
;;

let to_js
  { version
  ; id
  ; sender_id
  ; app_metadata
  ; public_key
  ; network
  ; scopes
  ; threshold
  ; notification
  }
  =
  let message = Beacon_base_message.{ version; id; sender_id } in
  let obj = Beacon_base_message.to_js message in
  let open Nightmare_js.Option in
  let open Preface.Fun.Infix in
  object%js
    val version = obj##.version
    val _id = obj##._id
    val senderId = obj##.senderId
    val appMetadata = App_metadata.to_js app_metadata
    val publicKey = Js.string public_key
    val network = Network.to_js network

    val scopes =
      Util.list_to_js_with (Js.string % Permission_scope.to_string) scopes

    val threshold = Threshold.to_js <$> threshold |> to_optdef
    val notification = Notification.to_js <$> notification |> to_optdef
  end
;;
