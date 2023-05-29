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
  | Sign
  | Operation_request
  | Encrypt
  | Notification
  | Threshold

let to_string = function
  | Sign -> "sign"
  | Operation_request -> "operation_request"
  | Encrypt -> "encrypt"
  | Notification -> "notification"
  | Threshold -> "threshold"
;;

let from_string str =
  match Util.normalize str with
  | "sign" -> Some Sign
  | "operation_request" -> Some Operation_request
  | "encrypt" -> Some Encrypt
  | "notification" -> Some Notification
  | "threshold" -> Some Threshold
  | _ -> None
;;

let from_js_array js_array =
  let open Preface.Fun.Infix in
  js_array
  |> Util.list_from_js_with (from_string % Js.to_string)
  |> Util.List_option.sequence
  |> Option.value ~default:[]
;;
