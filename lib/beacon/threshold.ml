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
  { amount : Yourbones_common.Tez.t
  ; timeframe : int64
  }

let from_js threshold =
  let open Yourbones_common in
  let amount =
    match Js.to_string threshold##.amount |> Tez.from_string with
    | Ok x -> x
    | Error _ -> 1000m
  in
  let timeframe =
    match Js.to_string threshold##.timeframe |> Int64.of_string_opt with
    | None -> 3600L
    | Some x -> x
  in
  { amount; timeframe }
;;

let to_js { amount; timeframe } =
  object%js
    val amount =
      Format.asprintf "%a" (Yourbones_common.Tez.pp ()) amount |> Js.string

    val timeframe = Int64.to_string timeframe |> Js.string
  end
;;
