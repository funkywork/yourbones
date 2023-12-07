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

type t = Z.t

let from_int x = if x < 0 then None else Some (Z.of_int x)
let from_int64 x = if Int64.compare 0L x < 0 then None else Some (Z.of_int64 x)
let from_z x = if Z.compare Z.zero x < 0 then None else Some x

let from_string x =
  x |> Int64.of_string_opt |> fun x -> Option.bind x from_int64
;;

let abs x = Int.abs x |> Z.of_int
let abs_64 x = Int64.abs x |> Z.of_int64
let abs_z x = Z.abs x
let abs_string x = x |> Int64.of_string_opt |> Option.map abs_64
let to_int x = Z.to_int x
let to_int64 x = Z.to_int64 x
let to_z x = x
let to_string x = Z.to_string x
let compare x y = Z.compare x y
let equal x y = Z.equal x y
let min x y = Z.min x y
let max x y = Z.max x y
let zero = Z.zero
let one = Z.one
let succ x = Z.succ x
let pred x = Z.pred x

let bounded_pred x =
  let x = Z.pred x in
  if Z.compare Z.zero x < 0 then zero else x
;;

let add x y = Z.(x + y)
let sub x y = Z.(x - y)
let bounded_sub x y = if Z.compare y x > 0 then zero else Z.(x - y)
let mul x y = Z.(x * y)

let ediv x divisor =
  if Z.equal divisor zero then None else Some (Z.ediv_rem x divisor)
;;

let pp = Z.pp_print

module Infix = struct
  let ( = ) x y = Z.equal x y
  let ( <> ) x y = not (x = y)
  let ( > ) x y = Z.compare x y > 0
  let ( >= ) x y = Z.compare x y >= 0
  let ( < ) x y = Z.compare x y < 0
  let ( <= ) x y = Z.compare x y <= 0
  let ( + ) x y = add x y
  let ( - ) x y = sub x y
  let ( -^ ) x y = bounded_sub x y
  let ( * ) x y = mul x y
end

include Infix

let encoding = Data_encoding.z
