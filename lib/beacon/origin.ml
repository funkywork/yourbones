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

type kind =
  | Website
  | Extension
  | P2p
  | Walletconnect

type t =
  { type_ : kind
  ; id : string
  }

let kind_to_string = function
  | Website -> "website"
  | Extension -> "extension"
  | P2p -> "p2p"
  | Walletconnect -> "walletconnect"
;;

let kind_from_string str =
  match Util.normalize str with
  | "website" -> Some Website
  | "extension" -> Some Extension
  | "p2p" -> Some P2p
  | "walletconnect" -> Some Walletconnect
  | _ -> None
;;

let from_js origin =
  let type_ =
    match origin##._type |> Js.to_string |> kind_from_string with
    | Some x -> x
    | None -> Website (* Should not happen! *)
  in
  let id = Js.to_string origin##.id in
  { type_; id }
;;

let to_js { type_; id } =
  object%js
    val _type = type_ |> kind_to_string |> Js.string
    val id = Js.string id
  end
;;
