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

open Yourbones_common
open Nightmare_test

let tez_testable = Alcotest.testable (Tez.pp ()) Tez.equal
let tez_error_testable = Alcotest.testable Tez.pp_error Tez.equal_error

module Result = Preface.Result.Monad (struct
  type t =
    [ `Tez_negative_amount of int64
    | `Tez_overflow
    | `Tez_invalid_string_representation of string
    | `Tez_invalid_multiplicator of int64
    | `Tez_invalid_divisor of int64
    ]
end)

let test_from_int_1 =
  test_equality
    ~about:"from_int"
    ~desc:"when the given int is valid, it should wrap it into a [Tez]"
    Alcotest.(result string tez_error_testable)
    (fun () ->
      let expected = Ok "640.000000"
      and computed =
        let open Result.Syntax in
        let+ res = Tez.from_int 640 in
        Format.asprintf "%a" (Tez.pp ()) res
      in
      expected, computed)
;;

let test_from_int64_1_micro =
  test_equality
    ~about:"Micro.from_int64"
    ~desc:"when the given int64 is valid, it should wrap it into a [Tez]"
    Alcotest.(result string tez_error_testable)
    (fun () ->
      let expected = Ok "640.459268"
      and computed =
        let open Result.Syntax in
        let+ res = Tez.Micro.from_int64 640459268L in
        Format.asprintf "%a" (Tez.pp ()) res
      in
      expected, computed)
;;

let test_from_int64 =
  test_equality
    ~about:"from_int64"
    ~desc:"test [from_int64] with different test cases"
    Alcotest.(list (result int64 tez_error_testable))
    (fun () ->
      let expected =
        [ Ok 22_000_000L
        ; Ok Int64.max_int
        ; Error `Tez_overflow
        ; Error (`Tez_negative_amount (-230L))
        ]
      and computed =
        List.map
          (Result.map Tez.to_int64)
          [ Tez.from_int64 22L
          ; Tez.Micro.from_int64 Int64.max_int
          ; Tez.from_int64 Int64.max_int
          ; Tez.from_int64 (-230L)
          ]
      in
      expected, computed)
;;

let test_from_succ =
  test_equality
    ~about:"succ"
    ~desc:"test [succ x] should increase [x] with [1tez]"
    Alcotest.(result string tez_error_testable)
    (fun () ->
      let expected = Ok "641.000000"
      and computed =
        let open Result.Syntax in
        let* value = Tez.from_int 640 in
        let+ res = Tez.succ value in
        Format.asprintf "%a" (Tez.pp ()) res
      in
      expected, computed)
;;

let test_from_pred =
  test_equality
    ~about:"pred"
    ~desc:"test [pred x] should decrease [x] with [1tez]"
    Alcotest.(result string tez_error_testable)
    (fun () ->
      let expected = Ok "639.000000"
      and computed =
        let open Result.Syntax in
        let* value = Tez.from_int 640 in
        let+ res = Tez.pred value in
        Format.asprintf "%a" (Tez.pp ()) res
      in
      expected, computed)
;;

let test_from_succ_micro =
  test_equality
    ~about:"Micro.succ"
    ~desc:"test [succ x] should increase [x] with [1mutez]"
    Alcotest.(result string tez_error_testable)
    (fun () ->
      let expected = Ok "640.000002"
      and computed =
        let open Result.Syntax in
        let* value = Tez.from_int 640 in
        let* res = Tez.Micro.succ value in
        let+ res = Tez.Micro.succ res in
        Format.asprintf "%a" (Tez.pp ()) res
      in
      expected, computed)
;;

let test_from_pred_micro =
  test_equality
    ~about:"Micro.pred"
    ~desc:"test [pred x] should decrease [x] with [1mutez]"
    Alcotest.(result string tez_error_testable)
    (fun () ->
      let expected = Ok "639.999998"
      and computed =
        let open Result.Syntax in
        let* value = Tez.from_int 640 in
        let* res = Tez.Micro.pred value in
        let+ res = Tez.Micro.pred res in
        Format.asprintf "%a" (Tez.pp ()) res
      in
      expected, computed)
;;

let test_from_pred_with_negative_amount =
  test_equality
    ~about:"pred"
    ~desc:
      "test [pred x] should raise an error if the resulted amount is negative"
    Alcotest.(result tez_testable tez_error_testable)
    (fun () ->
      let expected = Error (`Tez_negative_amount (-1000000L))
      and computed =
        let open Result.Syntax in
        let* value = Tez.from_int 0 in
        Tez.pred value
      in
      expected, computed)
;;

let test_add_succeed =
  test_equality
    ~about:"add"
    ~desc:"test add without overflow"
    (Alcotest.result tez_testable tez_error_testable)
    (fun () ->
    let expected = Tez.from_int 42
    and computed =
      let open Result.Syntax in
      let* a = Tez.from_int 20 in
      let* b = Tez.from_int 22 in
      Tez.add a b
    in
    expected, computed)
;;

let test_add_succeed_with_different_basis =
  test_equality
    ~about:"add"
    ~desc:"test add without overflow and multiple basis"
    (Alcotest.result tez_testable tez_error_testable)
    (fun () ->
    let expected = Tez.Micro.from_int 20_000_053
    and computed =
      let open Result.Syntax in
      let* a = Tez.from_int 20 in
      let* b = Tez.Micro.from_int 22 in
      let* c = Tez.Micro.from_int 31 in
      let* x = Tez.add a b in
      Tez.add x c
    in
    expected, computed)
;;

let test_add_succeed_with_different_basis_using_infix =
  test_equality
    ~about:"|+|"
    ~desc:"test add without overflow using infix operators"
    (Alcotest.result tez_testable tez_error_testable)
    (fun () ->
    let expected = Tez.Micro.from_int 20_000_053
    and computed =
      let open Tez.Infix in
      Tez.from_int 20 |+| Tez.Micro.from_int 22 |+| Tez.Micro.from_int 31
    in
    expected, computed)
;;

let test_add_with_overflow =
  test_equality
    ~about:"add"
    ~desc:"test add with overflow"
    (Alcotest.result tez_testable tez_error_testable)
    (fun () ->
    let expected = Error `Tez_overflow
    and computed =
      let open Result.Syntax in
      let* a = Tez.Micro.from_int64 Int64.(sub max_int 2L) in
      let* b = Tez.from_int 4 in
      Tez.add a b
    in
    expected, computed)
;;

let test_sub_succeed =
  test_equality
    ~about:"sub"
    ~desc:"test sub without overflow"
    (Alcotest.result tez_testable tez_error_testable)
    (fun () ->
    let expected = Tez.from_int 42
    and computed =
      let open Result.Syntax in
      let* a = Tez.from_int 64 in
      let* b = Tez.from_int 22 in
      Tez.sub a b
    in
    expected, computed)
;;

let test_sub_succeed_with_different_basis =
  test_equality
    ~about:"sub"
    ~desc:"test sub without overflow and multiple basis"
    (Alcotest.result tez_testable tez_error_testable)
    (fun () ->
    let expected = Tez.Micro.from_int 19_999_947
    and computed =
      let open Result.Syntax in
      let* a = Tez.from_int 20 in
      let* b = Tez.Micro.from_int 22 in
      let* c = Tez.Micro.from_int 31 in
      let* x = Tez.sub a b in
      Tez.sub x c
    in
    expected, computed)
;;

let test_sub_succeed_with_different_basis_using_infix =
  test_equality
    ~about:"|-|"
    ~desc:"test sub without overflow using infix operators"
    (Alcotest.result tez_testable tez_error_testable)
    (fun () ->
    let expected = Tez.Micro.from_int 19_999_947
    and computed =
      let open Tez.Infix in
      Tez.from_int 20 |-| Tez.Micro.from_int 22 |-| Tez.Micro.from_int 31
    in
    expected, computed)
;;

let test_sub_with_underflow =
  test_equality
    ~about:"sub"
    ~desc:"test sub with underflow"
    (Alcotest.result tez_testable tez_error_testable)
    (fun () ->
    let expected = Error (`Tez_negative_amount (-1L))
    and computed =
      let open Result.Syntax in
      let* a = Tez.Micro.from_int64 1L in
      let* b = Tez.Micro.from_int 2 in
      Tez.sub a b
    in
    expected, computed)
;;

let test_mul_succeed =
  test_equality
    ~about:"mul"
    ~desc:"test mul valid"
    (Alcotest.result tez_testable tez_error_testable)
    (fun () ->
    let expected = Tez.from_int 220
    and computed =
      let open Result.Syntax in
      let* a = Tez.from_int 22 in
      let b = 10L in
      Tez.mul a b
    in
    expected, computed)
;;

let test_mul_micro_succeed =
  test_equality
    ~about:"mul"
    ~desc:"test mul with micro valid"
    (Alcotest.result tez_testable tez_error_testable)
    (fun () ->
    let expected = Tez.Micro.from_int 220
    and computed =
      let open Result.Syntax in
      let* a = Tez.Micro.from_int 22 in
      let b = 10L in
      Tez.mul a b
    in
    expected, computed)
;;

let test_mul_negative_multiplicator =
  test_equality
    ~about:"mul"
    ~desc:"test mul invalid multiplicator"
    (Alcotest.result tez_testable tez_error_testable)
    (fun () ->
    let expected = Error (`Tez_invalid_multiplicator (-2L))
    and computed =
      let open Result.Syntax in
      let* a = Tez.from_int 22 in
      let b = -2L in
      Tez.mul a b
    in
    expected, computed)
;;

let test_mul_overflow =
  test_equality
    ~about:"mul"
    ~desc:"test mul overflow"
    (Alcotest.result tez_testable tez_error_testable)
    (fun () ->
    let expected = Error `Tez_overflow
    and computed =
      let open Result.Syntax in
      let* a = Tez.from_int64 Int64.(div max_int 2L |> succ) in
      let b = 2L in
      Tez.mul a b
    in
    expected, computed)
;;

let test_div_valid =
  test_equality
    ~about:"div"
    ~desc:"test div with succeed"
    (Alcotest.result tez_testable tez_error_testable)
    (fun () ->
    let expected = Tez.from_int 210
    and computed =
      let open Result.Syntax in
      let* a = Tez.from_int 420 in
      let b = 2L in
      Tez.div a b
    in
    expected, computed)
;;

let test_div_with_null_divisor =
  test_equality
    ~about:"div"
    ~desc:"test div with null division"
    (Alcotest.result tez_testable tez_error_testable)
    (fun () ->
    let expected = Error (`Tez_invalid_divisor 0L)
    and computed =
      let open Result.Syntax in
      let* a = Tez.from_int 420 in
      let b = 0L in
      Tez.div a b
    in
    expected, computed)
;;

let test_div_with_negative_divisor =
  test_equality
    ~about:"div"
    ~desc:"test div with negative division"
    (Alcotest.result tez_testable tez_error_testable)
    (fun () ->
    let expected = Error (`Tez_invalid_divisor (-30L))
    and computed =
      let open Result.Syntax in
      let* a = Tez.from_int 420 in
      let b = -30L in
      Tez.div a b
    in
    expected, computed)
;;

let test_from_string_1 =
  test_equality
    ~about:"from_string"
    ~desc:"use from_string for producing a tez"
    (Alcotest.result tez_testable tez_error_testable)
    (fun () ->
    let expected = Tez.(from_int 20 |+| Micro.from_int 112)
    and computed = Tez.from_string "20.000112" in
    expected, computed)
;;

let test_from_string_2 =
  test_equality
    ~about:"from_string"
    ~desc:"use from_string for producing a tez"
    (Alcotest.result tez_testable tez_error_testable)
    (fun () ->
    let expected = Tez.(from_int 2000007)
    and computed = Tez.from_string "2000007" in
    expected, computed)
;;

let test_from_string_3 =
  test_equality
    ~about:"from_string"
    ~desc:"use from_string for producing a tez"
    (Alcotest.result tez_testable tez_error_testable)
    (fun () ->
    let expected = Error (`Tez_invalid_string_representation "-2000007")
    and computed = Tez.from_string "-2000007" in
    expected, computed)
;;

let test_from_string_4 =
  test_equality
    ~about:"from_string"
    ~desc:"use from_string for producing a tez"
    (Alcotest.result tez_testable tez_error_testable)
    (fun () ->
    let expected = Error (`Tez_invalid_string_representation "2000007.ffoo-bar")
    and computed = Tez.from_string "2000007.ffoo-bar" in
    expected, computed)
;;

let test_from_string_5 =
  test_equality
    ~about:"from_string"
    ~desc:"use from_string for producing a tez"
    (Alcotest.result tez_testable tez_error_testable)
    (fun () ->
    let expected = Error (`Tez_invalid_string_representation "2000007.-25")
    and computed = Tez.from_string "2000007.-25" in
    expected, computed)
;;

let test_infix_comparator =
  test_equality
    ~about:"infix operators"
    ~desc:"test infix operators"
    Alcotest.(list (result bool tez_error_testable))
    (fun () ->
      let inf op x y =
        let open Result.Syntax in
        let* x = x in
        let+ y = y in
        op x y
      in
      let ( =? ) = inf Tez.( = )
      and ( <>? ) = inf Tez.( <> )
      and ( >? ) = inf Tez.( > )
      and ( >=? ) = inf Tez.( >= )
      and ( <? ) = inf Tez.( < )
      and ( <=? ) = inf Tez.( <= ) in
      let expected =
        [ Ok true
        ; Ok false
        ; Ok true
        ; Ok false
        ; Ok true
        ; Ok false
        ; Ok false
        ; Ok true
        ; Ok false
        ; Ok true
        ; Ok false
        ; Ok true
        ; Ok false
        ; Ok false
        ; Ok true
        ; Ok true
        ]
      and computed =
        Tez.
          [ from_int 1 =? from_int 1
          ; from_int 1 =? from_int 2
          ; from_int 1 <>? from_int 2
          ; from_int 2 <>? from_int 2
          ; from_int 2 >? from_int 1
          ; from_int 1 >? from_int 2
          ; from_int 1 >? from_int 1
          ; from_int 2 >=? from_int 1
          ; from_int 1 >=? from_int 2
          ; from_int 1 >=? from_int 1
          ; from_int 2 <? from_int 1
          ; from_int 1 <? from_int 2
          ; from_int 1 <? from_int 1
          ; from_int 2 <=? from_int 1
          ; from_int 1 <=? from_int 2
          ; from_int 1 <=? from_int 1
          ]
      in
      expected, computed)
;;

let cases =
  ( "Tez"
  , [ test_from_int_1
    ; test_from_int64_1_micro
    ; test_from_int64
    ; test_from_succ
    ; test_from_pred
    ; test_from_succ_micro
    ; test_from_pred_micro
    ; test_from_pred_with_negative_amount
    ; test_add_succeed
    ; test_add_succeed_with_different_basis
    ; test_add_succeed_with_different_basis_using_infix
    ; test_add_with_overflow
    ; test_sub_succeed
    ; test_sub_succeed_with_different_basis
    ; test_sub_succeed_with_different_basis_using_infix
    ; test_sub_with_underflow
    ; test_mul_succeed
    ; test_mul_micro_succeed
    ; test_mul_negative_multiplicator
    ; test_mul_overflow
    ; test_div_valid
    ; test_div_with_null_divisor
    ; test_div_with_negative_divisor
    ; test_from_string_1
    ; test_from_string_2
    ; test_from_string_3
    ; test_from_string_4
    ; test_from_string_5
    ; test_infix_comparator
    ] )
;;
