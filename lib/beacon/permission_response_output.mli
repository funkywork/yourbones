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

(** The result of a permissions request (to link a Wallet to Beacon). *)

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
  ; address : Yourbones.Address.kind Yourbones.Address.t
  ; account_info : Account_info.t
  ; wallet_key : string option
  }

val from_js : Bindings.permission_response_output Js.t -> t
val to_js : t -> Bindings.permission_response_output Js.t
