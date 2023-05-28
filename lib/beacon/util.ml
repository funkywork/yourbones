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

let normalize str = String.(trim @@ lowercase_ascii str)

let array_to_js_with f arr =
  let array = new%js Js.array_empty in
  let () =
    Array.fold_left
      (fun () x ->
        let _ = array##push (f x) in
        ())
      ()
      arr
  in
  array
;;

let array_to_js arr = Js.array arr

let list_to_js_with f list =
  let array = new%js Js.array_empty in
  let () =
    List.fold_left
      (fun () x ->
        let _ = array##push (f x) in
        ())
      ()
      list
  in
  array
;;

let list_to_js list = list_to_js_with (fun x -> x) list

let array_from_js_with f arr =
  let length = arr##.length in
  Array.init length (fun i ->
    match Js.array_get arr i |> Js.Optdef.to_option with
    | Some x -> f x
    | None -> (* unreacheable *) assert false)
;;

let array_from_js arr = Js.to_array arr

let list_from_js_with f arr =
  let length = arr##.length in
  List.init length (fun i ->
    match Js.array_get arr i |> Js.Optdef.to_option with
    | Some x -> f x
    | None -> (* unreacheable *) assert false)
;;

let list_from_js arr = list_from_js_with (fun x -> x) arr
