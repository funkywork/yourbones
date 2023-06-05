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
  ; app_metadata : App_metadata.t option
  ; public_key : string
  ; network : Network.t
  ; scopes : Permission_scope.t list
  ; threshold : Threshold.t option
  ; notification : Notification.t option
  ; address : Yourbones.Address.t
  ; account_info : Account_info.t
  ; wallet_key : string option
  }

let from_js response =
  let Permission_response.
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
    Permission_response.from_js response
  in
  let open Nightmare_js.Undefinable in
  let address =
    Js.to_string response##.address |> Yourbones.Address.from_string'
  in
  let account_info = Account_info.from_js response##.accountInfo in
  let wallet_key = Js.to_string <$> response##.walletKey |> to_option in
  { version
  ; id
  ; sender_id
  ; app_metadata
  ; public_key
  ; network
  ; scopes
  ; threshold
  ; notification
  ; address
  ; account_info
  ; wallet_key
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
  ; address
  ; account_info
  ; wallet_key
  }
  =
  let response =
    Permission_response.
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
  in
  let obj = Permission_response.to_js response in
  let open Nightmare_js.Option in
  object%js
    val version = obj##.version
    val _id = obj##._id
    val senderId = obj##.senderId
    val appMetadata = obj##.appMetadata
    val publicKey = obj##.publicKey
    val network = obj##.network
    val scopes = obj##.scopes
    val threshold = obj##.threshold
    val notification = obj##.notification
    val address = Js.string (address |> Yourbones.Address.to_string)
    val accountInfo = Account_info.to_js account_info
    val walletKey = Js.string <$> wallet_key |> to_optdef
  end
;;
