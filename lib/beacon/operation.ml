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
open Nightmare_js

module Kind = struct
  type t = Transaction

  let to_string = function
    | Transaction -> "transaction"
  ;;
end

module Partial = struct
  type t =
    { kind : Kind.t
    ; source : Yourbones.Address.t option
    ; fee : Yourbones.Tez.t option
    ; counter : Z.t option
    ; gas_limit : Z.t option
    ; storage_limit : Z.t option
    ; amount : Yourbones.Tez.t option
    ; destination : Yourbones.Address.t option
    }

  let forge
    kind
    ?source
    ?fee
    ?counter
    ?gas_limit
    ?storage_limit
    ?amount
    ?destination
    ()
    =
    { kind
    ; source
    ; fee
    ; counter
    ; gas_limit
    ; storage_limit
    ; amount
    ; destination
    }
  ;;

  let to_js
    { kind
    ; source
    ; fee
    ; counter
    ; gas_limit
    ; storage_limit
    ; amount
    ; destination
    }
    =
    let open Option in
    let open Preface.Fun.Infix in
    object%js
      val kind = Kind.to_string kind |> Js.string

      val source =
        Js.string % Yourbones.Address.to_string <$> source |> to_optdef

      val fee =
        Js.string % Int64.to_string % Yourbones.Tez.to_mutez
        <$> fee
        |> to_optdef

      val counter = Js.string % Z.to_string <$> counter |> to_optdef
      val gas_limit = Js.string % Z.to_string <$> gas_limit |> to_optdef
      val storage_limit = Js.string % Z.to_string <$> storage_limit |> to_optdef

      val amount =
        Js.string % Int64.to_string % Yourbones.Tez.to_mutez
        <$> amount
        |> to_optdef

      val destination =
        Js.string % Yourbones.Address.to_string <$> destination |> to_optdef
    end
  ;;
end

type t = Partial.t

let to_js = Partial.to_js

let batch operations =
  let details = Util.list_to_js_with to_js operations in
  object%js
    val operationDetails = details
  end
;;

module Transaction = struct
  let forge ?source ?fee ?counter ?gas_limit ?storage_limit ~destination amount =
    Partial.forge
      Kind.Transaction
      ?source
      ?fee
      ?counter
      ?gas_limit
      ?storage_limit
      ~destination
      ~amount
      ()
  ;;
end
