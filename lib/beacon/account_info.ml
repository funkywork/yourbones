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
  { address : Yourbones.Address.kind Yourbones.Address.t
  ; network : Network.t
  ; scopes : Permission_scope.t list
  ; threshold : Threshold.t option
  ; account_identifier : string
  ; sender_id : string
  ; origin : Origin.t
  ; wallet_key : string option
  ; connected_at : int
  ; notification : Notification.t option
  }

let from_js account_info =
  let Permission_entity.{ address; network; scopes; threshold } =
    Permission_entity.from_js account_info
  in
  let open Nightmare_js.Undefinable in
  let account_identifier = Js.to_string account_info##.accountIdentifier in
  let sender_id = Js.to_string account_info##.senderId in
  let origin = Origin.from_js account_info##.origin in
  let wallet_key = Js.to_string <$> account_info##.walletKey |> to_option in
  let connected_at = account_info##.connectedAt in
  let notification =
    Notification.from_js <$> account_info##.notification |> to_option
  in
  { address
  ; network
  ; scopes
  ; threshold
  ; account_identifier
  ; sender_id
  ; origin
  ; wallet_key
  ; connected_at
  ; notification
  }
;;

let to_js
  { address
  ; network
  ; scopes
  ; threshold
  ; account_identifier
  ; sender_id
  ; origin
  ; wallet_key
  ; connected_at
  ; notification
  }
  =
  let open Nightmare_js.Option in
  let p =
    Permission_entity.({ address; network; scopes; threshold } |> to_js)
  in
  object%js
    val address = p##.address
    val network = p##.network
    val scopes = p##.scopes
    val threshold = p##.threshold
    val accountIdentifier = Js.string account_identifier
    val senderId = Js.string sender_id
    val origin = Origin.to_js origin
    val walletKey = Js.string <$> wallet_key |> to_optdef
    val connectedAt = connected_at
    val notification = Notification.to_js <$> notification |> to_optdef
  end
;;
