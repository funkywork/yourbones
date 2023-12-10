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

type t = Int64.t

type error =
  [ `Tez_negative_amount of int64
  | `Tez_overflow
  | `Tez_invalid_string_representation of string
  | `Tez_invalid_divisor of Nat.t
  ]

exception Tez_exception of error

let symbol = "ꜩ"
let zero = 0L

let add x y =
  let res = Int64.add x y in
  if Int64.compare res x < 0 then Error `Tez_overflow else Ok res
;;

let sub x y =
  let res = Int64.sub x y in
  if Int64.compare res 0L < 0 then Error (`Tez_negative_amount res) else Ok res
;;

let mul x m =
  let m = Nat.to_int64 m in
  let comparison = Int64.compare m 0L in
  if Int.equal comparison 0
  then Ok Int64.zero
  else if Int64.compare x Int64.(div max_int m) > 0
  then Error `Tez_overflow
  else Ok (Int64.mul x m)
;;

let div x d =
  let nd = Nat.to_int64 d in
  if Int64.compare nd 0L <= 0
  then Error (`Tez_invalid_divisor d)
  else Ok (Int64.div x nd)
;;

module Make
    (Previous : sig
       type t = int64

       val one : t

       val from_int64
         :  int64
         -> (t, [> `Tez_negative_amount of int64 | `Tez_overflow ]) result
     end)
    (Current : sig
       val one : Previous.t
     end) =
struct
  type t = int64

  let one = Int64.mul Current.one Previous.one

  let from_int64 x =
    Result.bind (Previous.from_int64 x) (fun x ->
      mul Current.one (Nat.abs_64 x))
  ;;

  let from_int x = from_int64 (Int64.of_int x)
  let truncate x = Int64.div x one
  let succ x = add x one
  let pred x = sub x one
end

module Micro =
  Make
    (struct
      type t = Int64.t

      let one = 1L

      let from_int64 x =
        if Int64.compare x 0L < 0 then Error (`Tez_negative_amount x) else Ok x
      ;;
    end)
    (struct
      let one = 1L
    end)

include (
  Make
    (Micro)
    (struct
      let one = 1_000_000L
    end) :
      Interfaces.TEZ with type t := t)

let compare x y = Int64.compare x y
let equal x y = Int64.equal x y
let min x y = Int64.min x y
let max x y = Int64.max x y
let to_int64 x = x
let to_mutez x = Micro.truncate x
let from_mutez x = Micro.from_int64 x

let from_mutez' x =
  match Micro.from_int64 x with
  | Ok x -> x
  | Error err -> raise (Tez_exception err)
;;

module Infix = struct
  let ( = ) x y = Int64.equal x y
  let ( <> ) x y = not (x = y)
  let ( > ) x y = Int64.compare x y > 0
  let ( >= ) x y = Int64.compare x y >= 0
  let ( < ) x y = Int64.compare x y < 0
  let ( <= ) x y = Int64.compare x y <= 0
  let ( + ) x y = add x y
  let ( - ) x y = sub x y
  let ( * ) x y = mul x y
  let ( / ) x y = div x y
  let l_op op x y = Result.bind x (fun i -> op i y)
  let r_op op x y = Result.bind y (fun i -> op x i)
  let b_op op x y = Result.bind x (fun x -> Result.bind y (fun y -> op x y))
  let ( |+ ) = l_op add
  let ( +| ) = r_op add
  let ( |+| ) = b_op add
  let ( |- ) = l_op sub
  let ( -| ) = r_op sub
  let ( |-| ) = b_op sub
  let ( |* ) x y = l_op mul x y
  let ( *| ) x y = l_op mul y x
  let ( |/ ) = l_op div
end

include Infix

let pp ?(floating_part = `Six) () ppf value =
  let left = Int64.div value one in
  match floating_part with
  | `None -> Format.fprintf ppf "%Li" left
  | `One ->
    let divider = 100_000L in
    let right = Int64.div (Int64.rem value one) divider in
    Format.fprintf ppf "%Li.%01Li" left right
  | `Two ->
    let divider = 10_000L in
    let right = Int64.div (Int64.rem value one) divider in
    Format.fprintf ppf "%Li.%02Li" left right
  | `Three ->
    let divider = 1000L in
    let right = Int64.div (Int64.rem value one) divider in
    Format.fprintf ppf "%Li.%03Li" left right
  | `Four ->
    let divider = 100L in
    let right = Int64.div (Int64.rem value one) divider in
    Format.fprintf ppf "%Li.%04Li" left right
  | `Five ->
    let divider = 10L in
    let right = Int64.div (Int64.rem value one) divider in
    Format.fprintf ppf "%Li.%05Li" left right
  | `Six ->
    let divider = Micro.one in
    let right = Int64.div (Int64.rem value one) divider in
    Format.fprintf ppf "%Li.%06Li" left right
;;

let pp_print_with ?(floating_part = `Six) () ppf value =
  let pp = pp ~floating_part () in
  Format.fprintf ppf "%a%s" pp value symbol
;;

let pp_print ppf value =
  let pp = pp () in
  Format.fprintf ppf "%a%s" pp value symbol
;;

let decimal_part_from_string full_repr decimal =
  let part =
    match String.length decimal with
    | 0 -> Some 0L
    | 1 -> Some 100_000L
    | 2 -> Some 10_000L
    | 3 -> Some 1000L
    | 4 -> Some 100L
    | 5 -> Some 10L
    | 6 -> Some 1L
    | _ -> None
  in
  match part, Int64.of_string_opt decimal with
  | Some part, Some decimal -> Micro.from_int64 (Int64.mul part decimal)
  | _ -> Error (`Tez_invalid_string_representation full_repr)
;;

let from_string str =
  (match String.(split_on_char '.' @@ trim str) with
   | [ integral ] | [ integral; "" ] ->
     Option.fold
       ~none:(Error (`Tez_invalid_string_representation str))
       ~some:from_int64
       (Int64.of_string_opt integral)
   | [ integral; decimal ] ->
     Option.fold
       ~none:(Error (`Tez_invalid_string_representation str))
       ~some:(fun integral ->
         let integral = from_int64 integral
         and decimal = decimal_part_from_string str decimal in
         Infix.(integral |+| decimal))
       (Int64.of_string_opt integral)
   | _ -> Error (`Tez_invalid_string_representation str))
  |> Result.map_error (fun _ -> `Tez_invalid_string_representation str)
;;

let pp_error ppf = function
  | `Tez_negative_amount x -> Format.fprintf ppf "`Tez_negative_amount (%Li)" x
  | `Tez_overflow -> Format.fprintf ppf "`Tez_overflow"
  | `Tez_invalid_string_representation s ->
    Format.fprintf ppf "`Tez_invalid_string_representation (\"%s\")" s
  | `Tez_invalid_divisor x ->
    Format.fprintf ppf "`Tez_invalid_divisor (%a)" Nat.pp x
;;

let equal_error a b =
  match a, b with
  | `Tez_negative_amount x, `Tez_negative_amount y -> Int64.equal x y
  | `Tez_invalid_divisor x, `Tez_invalid_divisor y -> Nat.equal x y
  | `Tez_overflow, `Tez_overflow -> true
  | `Tez_invalid_string_representation x, `Tez_invalid_string_representation y
    -> String.equal x y
  | _ -> false
;;

let encoding =
  let open Data_encoding in
  let decode = Z.of_int64
  and encode =
    Json.wrap_error (fun i ->
      match Micro.from_int64 (Z.to_int64 i) with
      | Ok x -> x
      | Error err -> raise @@ Tez_exception err)
  in
  def "mutez" (check_size 10 (conv decode encode n))
;;
