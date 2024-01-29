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

open Yourbones
open Nightmare_test

let nat_testable = Alcotest.testable Nat.pp Nat.equal

let test_form_int_pos =
  test_equality
    ~about:"from_int"
    ~desc:"when a valid integer is given it should return it wrapped into Some"
    Alcotest.(option nat_testable)
    (fun () ->
      let expected = Some (Nat.abs 10)
      and computed = Nat.from_int 10 in
      expected, computed)
;;

let test_form_int_neg =
  test_equality
    ~about:"from_int"
    ~desc:"when a negative integer is given it should return None"
    Alcotest.(option nat_testable)
    (fun () ->
      let expected = None
      and computed = Nat.from_int (-10) in
      expected, computed)
;;

let test_form_int64_pos =
  test_equality
    ~about:"from_int64"
    ~desc:
      "when a valid integer64 is given it should return it wrapped into Some"
    Alcotest.(option nat_testable)
    (fun () ->
      let expected = Some (Nat.abs_64 10000L)
      and computed = Nat.from_int64 10000L in
      expected, computed)
;;

let test_form_int64_neg =
  test_equality
    ~about:"from_int64"
    ~desc:"when a negative integer64 is given it should return None"
    Alcotest.(option nat_testable)
    (fun () ->
      let expected = None
      and computed = Nat.from_int64 (-100L) in
      expected, computed)
;;

let test_form_z_pos =
  test_equality
    ~about:"from_z"
    ~desc:"when a valid Z.t is given it should return it wrapped into Some"
    Alcotest.(option nat_testable)
    (fun () ->
      let expected = Some (Nat.abs_64 10000L)
      and computed = Nat.from_z (Z.of_int64 10000L) in
      expected, computed)
;;

let test_form_z_neg =
  test_equality
    ~about:"from_z"
    ~desc:"when a negative Z.t is given it should return None"
    Alcotest.(option nat_testable)
    (fun () ->
      let expected = None
      and computed = Nat.from_z (Z.of_int64 (-100L)) in
      expected, computed)
;;

let test_form_string_valid =
  test_equality
    ~about:"from_string"
    ~desc:"when a invalid string is given it should return it wrapped into Some"
    Alcotest.(option nat_testable)
    (fun () ->
      let expected = Some (Nat.abs 1345)
      and computed = Nat.from_string "1345" in
      expected, computed)
;;

let test_form_string_neg =
  test_equality
    ~about:"from_string"
    ~desc:
      "when a negative int represented as a string is given it should return \
       None"
    Alcotest.(option nat_testable)
    (fun () ->
      let expected = None
      and computed = Nat.from_string "-100" in
      expected, computed)
;;

let test_form_string_invalid =
  test_equality
    ~about:"from_string"
    ~desc:"when an invalid string is given it should return None"
    Alcotest.(option nat_testable)
    (fun () ->
      let expected = None
      and computed = Nat.from_string "mspmsp" in
      expected, computed)
;;

let test_abs_pos =
  test_equality
    ~about:"abs"
    ~desc:"when an integer is given, it should transform it into a nat"
    Alcotest.(option nat_testable)
    (fun () ->
      let expected = Nat.from_int 10
      and computed = Some (Nat.abs 10) in
      expected, computed)
;;

let test_abs_neg =
  test_equality
    ~about:"abs"
    ~desc:
      "when a negative integer is given, it should transform it into a nat \
       using absolute value"
    Alcotest.(option nat_testable)
    (fun () ->
      let expected = Nat.from_int 10
      and computed = Some (Nat.abs (-10)) in
      expected, computed)
;;

let test_abs_64_pos =
  test_equality
    ~about:"abs_64"
    ~desc:"when an integer64 is given, it should transform it into a nat"
    Alcotest.(option nat_testable)
    (fun () ->
      let expected = Nat.from_int64 10L
      and computed = Some (Nat.abs_64 10L) in
      expected, computed)
;;

let test_abs_64_neg =
  test_equality
    ~about:"abs_64"
    ~desc:
      "when a negative integer64 is given, it should transform it into a nat \
       using absolute value"
    Alcotest.(option nat_testable)
    (fun () ->
      let expected = Nat.from_int64 10L
      and computed = Some (Nat.abs_64 (-10L)) in
      expected, computed)
;;

let test_abs_z_pos =
  test_equality
    ~about:"abs_z"
    ~desc:"when an Z.t is given, it should transform it into a nat"
    Alcotest.(option nat_testable)
    (fun () ->
      let expected = Nat.from_z (Z.of_int 10)
      and computed = Some (Nat.abs_z (Z.of_int 10)) in
      expected, computed)
;;

let test_abs_z_neg =
  test_equality
    ~about:"abs_z"
    ~desc:
      "when a negative Z.t is given, it should transform it into a nat using \
       absolute value"
    Alcotest.(option nat_testable)
    (fun () ->
      let expected = Nat.from_z (Z.of_int 10)
      and computed = Some (Nat.abs_z (Z.of_int (-10))) in
      expected, computed)
;;

let test_abs_string_pos =
  test_equality
    ~about:"abs_string"
    ~desc:"when the string is a valid positive nat, it should wrap it into some"
    Alcotest.(option nat_testable)
    (fun () ->
      let expected = Nat.from_int 10
      and computed = Nat.abs_string "10" in
      expected, computed)
;;

let test_abs_string_neg =
  test_equality
    ~about:"abs_string"
    ~desc:
      "when the string is a valid negative nat, it should wrap the into some \
       using absolute value"
    Alcotest.(option nat_testable)
    (fun () ->
      let expected = Nat.from_int 10
      and computed = Nat.abs_string "-10" in
      expected, computed)
;;

let test_succ =
  test_equality
    ~about:"succ"
    ~desc:"compute the successor of a natural"
    nat_testable
    (fun () ->
       let expected = Nat.abs 12
       and computed = Nat.abs 10 |> Nat.succ |> Nat.succ in
       expected, computed)
;;

let test_pred_valid =
  test_equality
    ~about:"pred"
    ~desc:
      "compute the predecessor of a natural that does not reach negative amount"
    Alcotest.(option nat_testable)
    (fun () ->
      let expected = Nat.from_int 10
      and computed =
        Nat.abs 12
        |> Nat.pred
        |> Nat.from_z
        |> Option.map Nat.pred
        |> Fun.flip Option.bind @@ Nat.from_z
      in
      expected, computed)
;;

let test_pred_invalid =
  test_equality
    ~about:"pred"
    ~desc:"compute the predecessor of a natural that does reach negative amount"
    Alcotest.(option nat_testable)
    (fun () ->
      let expected = None
      and computed =
        Nat.abs 1
        |> Nat.pred
        |> Nat.from_z
        |> Option.map Nat.pred
        |> Fun.flip Option.bind @@ Nat.from_z
      in
      expected, computed)
;;

let test_bounded_pred_pos =
  test_equality
    ~about:"bounded_pred"
    ~desc:
      "compute the bounded predecessor of a natural that does not reach \
       negative amount"
    nat_testable
    (fun () ->
       let expected = Nat.abs 10
       and computed = Nat.abs 12 |> Nat.bounded_pred |> Nat.bounded_pred in
       expected, computed)
;;

let test_bounded_pred_neg =
  test_equality
    ~about:"bounded_pred"
    ~desc:
      "compute the bounded predecessor of a natural that does  reach negative \
       amount"
    nat_testable
    (fun () ->
       let expected = Nat.zero
       and computed = Nat.abs 1 |> Nat.bounded_pred |> Nat.bounded_pred in
       expected, computed)
;;

let test_some_addition_and_multiplication =
  test_equality
    ~about:"add & mul"
    ~desc:"Compute some arithmetic operation using add and mul"
    nat_testable
    (fun () ->
       let expected = Nat.abs 310
       and computed = Nat.(((abs 10 + abs 20) * abs 10) + (abs 5 * abs 2)) in
       expected, computed)
;;

let test_some_addition_and_multiplication_and_bounded_sub =
  test_equality
    ~about:"add & mul & bounded_sub"
    ~desc:"Compute some arithmetic operation using add and mul and bounded_sub"
    nat_testable
    (fun () ->
       let expected = Nat.abs 10
       and computed =
         Nat.(((abs 10 + abs 20) * abs 10) + (abs 5 * abs 2) -^ abs 300)
       in
       expected, computed)
;;

let test_ediv_rem_with_valid_divisor =
  test_equality
    ~about:"ediv_rem"
    ~desc:
      "when the divisor is valid, it should returns the result and the \
       remainder"
    Alcotest.(option @@ pair nat_testable nat_testable)
    (fun () ->
      let expected = Some Nat.(abs 2, abs 1)
      and computed = Nat.(ediv_rem (abs 11) (abs 5)) in
      expected, computed)
;;

let test_ediv_rem_with_invalid_divisor =
  test_equality
    ~about:"ediv_rem"
    ~desc:"when the divisor is not valid, it should returns None"
    Alcotest.(option @@ pair nat_testable nat_testable)
    (fun () ->
      let expected = None
      and computed = Nat.(ediv_rem (abs 11) (abs 0)) in
      expected, computed)
;;

let test_ediv_with_valid_divisor =
  test_equality
    ~about:"ediv"
    ~desc:"when the divisor is valid, it should returns the result"
    Alcotest.(option nat_testable)
    (fun () ->
      let expected = Nat.from_int 2
      and computed = Nat.(ediv (abs 11) (abs 5)) in
      expected, computed)
;;

let test_ediv_with_invalid_divisor =
  test_equality
    ~about:"ediv"
    ~desc:"when the divisor is not valid, it should returns None"
    Alcotest.(option nat_testable)
    (fun () ->
      let expected = None
      and computed = Nat.(ediv (abs 11) (abs 0)) in
      expected, computed)
;;

let test_rem_with_valid_divisor =
  test_equality
    ~about:"rem"
    ~desc:"when the divisor is valid, it should returns the remainder"
    Alcotest.(option nat_testable)
    (fun () ->
      let expected = Nat.from_int 1
      and computed = Nat.(rem (abs 11) (abs 5)) in
      expected, computed)
;;

let test_rem_with_invalid_divisor =
  test_equality
    ~about:"rem"
    ~desc:"when the divisor is not valid, it should returns None"
    Alcotest.(option nat_testable)
    (fun () ->
      let expected = None
      and computed = Nat.(rem (abs 11) (abs 0)) in
      expected, computed)
;;

let test_sub_pos =
  test_equality
    ~about:"sub"
    ~desc:
      "when the substraction does not reach a negative amount, it still \
       possible to project the result into a nat"
    Alcotest.(option nat_testable)
    (fun () ->
      let expected = Nat.from_int 10
      and computed = Nat.(sub (abs 13) (abs 3)) |> Nat.from_z in
      expected, computed)
;;

let test_sub_neg =
  test_equality
    ~about:"sub"
    ~desc:
      "when the substraction does reach a negative amount, it is not possible \
       to project the result into a nat"
    Alcotest.(option nat_testable)
    (fun () ->
      let expected = None
      and computed = Nat.(sub (abs 2) (abs 3)) |> Nat.from_z in
      expected, computed)
;;

let test_bounded_sub_pos =
  test_equality
    ~about:"sub"
    ~desc:
      "when the substraction does not reach a negative amount, the result does \
       not reach 0"
    nat_testable
    (fun () ->
       let expected = Nat.abs 10
       and computed = Nat.(bounded_sub (abs 13) (abs 3)) in
       expected, computed)
;;

let test_bounded_sub_neg =
  test_equality
    ~about:"sub"
    ~desc:
      "when the substraction does  reach a negative amount, the result should \
       be 0"
    nat_testable
    (fun () ->
       let expected = Nat.zero
       and computed = Nat.(bounded_sub (abs 2) (abs 3)) in
       expected, computed)
;;

let test_gt_t =
  test_equality ~about:">" ~desc:"test > operator true" Alcotest.bool (fun () ->
    true, Nat.(one > zero))
;;

let test_gt_f =
  test_equality
    ~about:">"
    ~desc:"test > operator false"
    Alcotest.bool
    (fun () -> false, Nat.(zero > one))
;;

let test_gte_t =
  test_equality
    ~about:">="
    ~desc:"test >= operator true"
    Alcotest.bool
    (fun () -> true, Nat.(one >= zero))
;;

let test_gte_t_equal =
  test_equality
    ~about:">="
    ~desc:"test >= operator true on equality"
    Alcotest.bool
    (fun () -> true, Nat.(one >= one))
;;

let test_gte_f =
  test_equality
    ~about:">="
    ~desc:"test >= operator false"
    Alcotest.bool
    (fun () -> false, Nat.(zero >= one))
;;

let test_lt_t =
  test_equality ~about:"<" ~desc:"test < operator true" Alcotest.bool (fun () ->
    true, Nat.(zero < one))
;;

let test_lt_f =
  test_equality
    ~about:"<"
    ~desc:"test < operator false"
    Alcotest.bool
    (fun () -> false, Nat.(one < zero))
;;

let test_lte_t =
  test_equality
    ~about:"<="
    ~desc:"test <= operator true"
    Alcotest.bool
    (fun () -> true, Nat.(zero <= one))
;;

let test_lte_t_equal =
  test_equality
    ~about:"<="
    ~desc:"test <= operator true on equality"
    Alcotest.bool
    (fun () -> true, Nat.(zero <= zero))
;;

let test_lte_f =
  test_equality
    ~about:"<="
    ~desc:"test <= operator false"
    Alcotest.bool
    (fun () -> false, Nat.(one <= zero))
;;

let cases =
  ( "Nat"
  , [ test_form_int_pos
    ; test_form_int_neg
    ; test_form_int64_pos
    ; test_form_int64_neg
    ; test_form_z_pos
    ; test_form_z_neg
    ; test_form_string_valid
    ; test_form_string_neg
    ; test_form_string_invalid
    ; test_abs_pos
    ; test_abs_neg
    ; test_abs_64_pos
    ; test_abs_64_neg
    ; test_abs_z_pos
    ; test_abs_z_neg
    ; test_abs_string_pos
    ; test_abs_string_neg
    ; test_succ
    ; test_pred_valid
    ; test_pred_invalid
    ; test_bounded_pred_pos
    ; test_bounded_pred_neg
    ; test_some_addition_and_multiplication
    ; test_some_addition_and_multiplication_and_bounded_sub
    ; test_ediv_rem_with_valid_divisor
    ; test_ediv_rem_with_invalid_divisor
    ; test_ediv_with_valid_divisor
    ; test_ediv_with_invalid_divisor
    ; test_rem_with_valid_divisor
    ; test_rem_with_invalid_divisor
    ; test_sub_pos
    ; test_sub_neg
    ; test_bounded_sub_pos
    ; test_bounded_sub_neg
    ; test_gt_t
    ; test_gt_f
    ; test_gte_t
    ; test_gte_t_equal
    ; test_gte_f
    ; test_lt_t
    ; test_lt_f
    ; test_lte_t
    ; test_lte_t_equal
    ; test_lte_f
    ] )
;;
