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

let hash_error_testable =
  Alcotest.testable Block_hash.pp_error Block_hash.equal_error
;;

module Result = Preface.Result.Monad (struct
  type t = Block_hash.error
end)

let test_from_string_valid_1 =
  test_equality
    ~about:"from_string"
    ~desc:
      "when the given string is valid it should returns a block_hash wrapped \
       into a Ok"
    Alcotest.(result string hash_error_testable)
    (fun () ->
      let open Result.Infix in
      let str = "BMVYm1HFyWUJ29rKEV7z2cAs49PXnFGEm54QVzkfoEdnLrGcD1P" in
      let expected = Ok str in
      let computed = str |> Block_hash.from_string >|= Block_hash.to_string in
      expected, computed)
;;

let test_from_string_invalid_1 =
  test_equality
    ~about:"from_string"
    ~desc:"when the given string is invalid it should returns an error"
    Alcotest.(result string hash_error_testable)
    (fun () ->
      let open Result.Infix in
      let str = "BMVYm1HFyWUJ29rKEV7z2cAs49PXnFGEm54QVzkfoEdnLrGcD1" in
      let expected = Error (`Block_hash_invalid_length str) in
      let computed = str |> Block_hash.from_string >|= Block_hash.to_string in
      expected, computed)
;;

let test_from_string_invalid_2 =
  test_equality
    ~about:"from_string"
    ~desc:"when the given string is invalid it should returns an error"
    Alcotest.(result string hash_error_testable)
    (fun () ->
      let open Result.Infix in
      let str = "BMVYm1HFyWUJ29rKEV7z2cAs49PXnFGEm54QVzkfoEdnLrGcD1K" in
      let expected = Error (`Block_hash_invalid_checksum str) in
      let computed = str |> Block_hash.from_string >|= Block_hash.to_string in
      expected, computed)
;;

let cases =
  ( "Block_hash"
  , [ test_from_string_valid_1
    ; test_from_string_invalid_1
    ; test_from_string_invalid_2
    ] )
;;
