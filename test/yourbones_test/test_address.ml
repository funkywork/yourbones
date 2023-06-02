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

let address_error_testable =
  Alcotest.testable Address.pp_error Address.equal_error
;;

module Result = Preface.Result.Monad (struct
  type t = Address.error
end)

let test_tz1_valid_address =
  test_equality
    ~about:"tz1"
    ~desc:"when the given address is valid it should wrap it into a Ok"
    Alcotest.(result string address_error_testable)
    (fun () ->
      let address = "tz1XxRjkB77R1rnxVRGQxmWAcG1cGQQeMAAL" in
      let expected = Ok address in
      let computed =
        Result.(Ok address >>= Address.tz1 >|= Address.to_string)
      in
      expected, computed)
;;

let test_tz1_invalid_address =
  test_equality
    ~about:"tz1"
    ~desc:"when the given address is invalid it should wrap it into an Error"
    Alcotest.(result string address_error_testable)
    (fun () ->
      let address = "tz1XxRjkB77R1rnxVRGQxmWAcG1cGQQeMALA" in
      let expected = Error (`Address_invalid_checksum address) in
      let computed = Result.(address |> Address.tz1 >|= Address.to_string) in
      expected, computed)
;;

let test_tz1_invalid_address_2 =
  test_equality
    ~about:"tz1"
    ~desc:
      "when the given address has not the right kind it should wrap the result \
       in an error"
    Alcotest.(result string address_error_testable)
    (fun () ->
      let address = "KT1Ap287P1NzsnToSJdA4aqSNjPomRaHBZSr" in
      let expected = Error (`Address_invalid_prefix address) in
      let computed = Result.(address |> Address.tz1 >|= Address.to_string) in
      expected, computed)
;;

let test_tz2_valid_address =
  test_equality
    ~about:"tz2"
    ~desc:"when the given address is valid it should wrap it into a Ok"
    Alcotest.(result string address_error_testable)
    (fun () ->
      let address = "tz2VGBaXuS6rnaa5hpC92qkgadRJKdEbeGwc" in
      let expected = Ok address in
      let computed = Result.(address |> Address.tz2 >|= Address.to_string) in
      expected, computed)
;;

let test_tz2_invalid_address =
  test_equality
    ~about:"tz2"
    ~desc:"when the given address is invalid it should wrap it into an Error"
    Alcotest.(result string address_error_testable)
    (fun () ->
      let address = "tz2VGBaXuS6rnaa5hpC92qkgadRJKdEbeGcw" in
      let expected = Error (`Address_invalid_checksum address) in
      let computed = Result.(address |> Address.tz2 >|= Address.to_string) in
      expected, computed)
;;

let test_tz2_invalid_address_2 =
  test_equality
    ~about:"tz2"
    ~desc:
      "when the given address has not the right kind it should wrap the result \
       in an error"
    Alcotest.(result string address_error_testable)
    (fun () ->
      let address = "tz1XxRjkB77R1rnxVRGQxmWAcG1cGQQeMAAL" in
      let expected = Error (`Address_invalid_prefix address) in
      let computed = Result.(address |> Address.tz2 >|= Address.to_string) in
      expected, computed)
;;

let test_tz3_valid_address =
  test_equality
    ~about:"tz3"
    ~desc:"when the given address is valid it should wrap it into a Ok"
    Alcotest.(result string address_error_testable)
    (fun () ->
      let address = "tz3WEJYwJ6pPwVbSL8FrSoAXRmFHHZTuEnMA" in
      let expected = Ok address in
      let computed = Result.(address |> Address.tz3 >|= Address.to_string) in
      expected, computed)
;;

let test_tz3_invalid_address =
  test_equality
    ~about:"tz3"
    ~desc:"when the given address is invalid it should wrap it into an Error"
    Alcotest.(result string address_error_testable)
    (fun () ->
      let address = "tz3WEJYwJ6pPwVbSL8FrSoAXRmFHHZTuEnAM" in
      let expected = Error (`Address_invalid_checksum address) in
      let computed = Result.(address |> Address.tz3 >|= Address.to_string) in
      expected, computed)
;;

let test_tz3_invalid_address_2 =
  test_equality
    ~about:"tz3"
    ~desc:
      "when the given address has not the right kind it should wrap the result \
       in an error"
    Alcotest.(result string address_error_testable)
    (fun () ->
      let address = "tz2VGBaXuS6rnaa5hpC92qkgadRJKdEbeGwc" in
      let expected = Error (`Address_invalid_prefix address) in
      let computed = Result.(address |> Address.tz3 >|= Address.to_string) in
      expected, computed)
;;

let test_tz4_valid_address =
  test_equality
    ~about:"tz4"
    ~desc:"when the given address is valid it should wrap it into a Ok"
    Alcotest.(result string address_error_testable)
    (fun () ->
      let address = "tz4HVR6aty9KwsQFHh81C1G7gBdhxT8kuytm" in
      let expected = Ok address in
      let computed = Result.(address |> Address.tz4 >|= Address.to_string) in
      expected, computed)
;;

let test_tz4_invalid_address =
  test_equality
    ~about:"tz4"
    ~desc:"when the given address is invalid it should wrap it into an Error"
    Alcotest.(result string address_error_testable)
    (fun () ->
      let address = "tz4HVR6aty9KwsQFHh81C1G7gBdhxT8kuymt" in
      let expected = Error (`Address_invalid_checksum address) in
      let computed = Result.(address |> Address.tz4 >|= Address.to_string) in
      expected, computed)
;;

let test_tz4_invalid_address_2 =
  test_equality
    ~about:"tz4"
    ~desc:
      "when the given address has not the right kind it should wrap the result \
       in an error"
    Alcotest.(result string address_error_testable)
    (fun () ->
      let address = "tz3WEJYwJ6pPwVbSL8FrSoAXRmFHHZTuEnMA" in
      let expected = Error (`Address_invalid_prefix address) in
      let computed = Result.(address |> Address.tz4 >|= Address.to_string) in
      expected, computed)
;;

let test_sr1_valid_address =
  test_equality
    ~about:"sr1"
    ~desc:"when the given address is valid it should wrap it into a Ok"
    Alcotest.(result string address_error_testable)
    (fun () ->
      let address = "sr1JPVatbbPoGp4vb6VfQ1jzEPMrYFcKq6VG" in
      let expected = Ok address in
      let computed = Result.(address |> Address.sr1 >|= Address.to_string) in
      expected, computed)
;;

let test_sr1_invalid_address =
  test_equality
    ~about:"sr1"
    ~desc:"when the given address is invalid it should wrap it into an Error"
    Alcotest.(result string address_error_testable)
    (fun () ->
      let address = "sr1JPVatbbPoGp4vb6VfQ1jzEPMrYFcKq6GV" in
      let expected = Error (`Address_invalid_checksum address) in
      let computed = Result.(address |> Address.sr1 >|= Address.to_string) in
      expected, computed)
;;

let test_sr1_invalid_address_2 =
  test_equality
    ~about:"sr1"
    ~desc:
      "when the given address has not the right kind it should wrap the result \
       in an error"
    Alcotest.(result string address_error_testable)
    (fun () ->
      let address = "tz4HVR6aty9KwsQFHh81C1G7gBdhxT8kuytm" in
      let expected = Error (`Address_invalid_prefix address) in
      let computed = Result.(address |> Address.sr1 >|= Address.to_string) in
      expected, computed)
;;

let test_kt1_valid_address =
  test_equality
    ~about:"kt1"
    ~desc:"when the given address is valid it should wrap it into a Ok"
    Alcotest.(result string address_error_testable)
    (fun () ->
      let address = "KT1RvwLgpxVv9ANCKsDb5vBgTaZRG1W4bKWP" in
      let expected = Ok address in
      let computed = Result.(address |> Address.kt1 >|= Address.to_string) in
      expected, computed)
;;

let test_kt1_invalid_address =
  test_equality
    ~about:"kt1"
    ~desc:"when the given address is invalid it should wrap it into an Error"
    Alcotest.(result string address_error_testable)
    (fun () ->
      let address = "KT1RvwLgpxVv9ANCKsDb5vBgTaZRG1W4bKPW" in
      let expected = Error (`Address_invalid_checksum address) in
      let computed = Result.(address |> Address.kt1 >|= Address.to_string) in
      expected, computed)
;;

let test_kt1_invalid_address_2 =
  test_equality
    ~about:"kt1"
    ~desc:
      "when the given address has not the right kind it should wrap the result \
       in an error"
    Alcotest.(result string address_error_testable)
    (fun () ->
      let address = "tz4HVR6aty9KwsQFHh81C1G7gBdhxT8kuytm" in
      let expected = Error (`Address_invalid_prefix address) in
      let computed = Result.(address |> Address.kt1 >|= Address.to_string) in
      expected, computed)
;;

let cases =
  ( "Address"
  , [ test_tz1_valid_address
    ; test_tz1_invalid_address
    ; test_tz1_invalid_address_2
    ; test_tz2_valid_address
    ; test_tz2_invalid_address
    ; test_tz2_invalid_address_2
    ; test_tz3_valid_address
    ; test_tz3_invalid_address
    ; test_tz3_invalid_address_2
    ; test_tz4_valid_address
    ; test_tz4_invalid_address
    ; test_tz4_invalid_address_2
    ; test_sr1_valid_address
    ; test_sr1_invalid_address
    ; test_sr1_invalid_address_2
    ; test_kt1_valid_address
    ; test_kt1_invalid_address
    ; test_kt1_invalid_address_2
    ] )
;;
