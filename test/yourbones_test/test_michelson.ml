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

let michelson_testable =
  Alcotest.testable
    (Micheline.Canonical.pp Michelson.pp)
    (Micheline.Canonical.equal Michelson.equal)
;;

let michelson_encoding = Micheline.canonical_encoding Michelson.encoding

let test_dummy_json_decoding =
  test_equality
    ~about:"encoding"
    ~desc:"A dummy test using encoding"
    Alcotest.(result michelson_testable string)
    (fun () ->
      let open Micheline in
      let open Michelson in
      let str =
        {json|{"prim":
             "Pair", "args":[
                 {"string":"foo"},
                 {"int":"12000000"},
                 {"prim":"False"},
                 {"prim":"Some","args":[{"int":"10"}]}]}
      |json}
      in
      let expected =
        Canonical.(
          from_node
          @@ prim
               d_pair
               [ string "foo"
               ; int 12000000
               ; prim d_false []
               ; prim d_some [ int 10 ]
               ])
      and computed =
        str
        |> Data_encoding.Json.from_string
        |> Result.map (Data_encoding.Json.destruct michelson_encoding)
      in
      Ok expected, computed)
;;

let cases = "Michelson", [ test_dummy_json_decoding ]
