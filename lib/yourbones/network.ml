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

module Type = struct
  type t =
    | Custom
    | Delphinet
    | Edonet
    | Florencenet
    | Granadanet
    | Hangzhounet
    | Ithacanet
    | Jakartanet
    | Kathmandunet
    | Limanet
    | Mumbainet
    | Nairobinet
    | Dailynet
    | Mondaynet
    | Ghostnet
    | Mainnet

  let to_string = function
    | Custom -> "custom"
    | Delphinet -> "delphinet"
    | Edonet -> "edonet"
    | Florencenet -> "florencenet"
    | Granadanet -> "granadanet"
    | Hangzhounet -> "hangzhounet"
    | Ithacanet -> "ithacanet"
    | Jakartanet -> "jakartanet"
    | Kathmandunet -> "kathmandunet"
    | Limanet -> "limanet"
    | Mumbainet -> "mumbainet"
    | Nairobinet -> "nairobinet"
    | Dailynet -> "dailynet"
    | Mondaynet -> "mondaynet"
    | Ghostnet -> "ghostnet"
    | Mainnet -> "mainnet"
  ;;

  let from_string str =
    match String.(trim @@ lowercase_ascii str) with
    | "delphinet" -> Some Delphinet
    | "edonet" -> Some Edonet
    | "florencenet" -> Some Florencenet
    | "granadanet" -> Some Granadanet
    | "hangzhounet" -> Some Hangzhounet
    | "ithacanet" -> Some Ithacanet
    | "jakartanet" -> Some Jakartanet
    | "kathmandunet" -> Some Kathmandunet
    | "limanet" -> Some Limanet
    | "dailynet" -> Some Dailynet
    | "mondaynet" -> Some Mondaynet
    | "ghostnet" -> Some Ghostnet
    | "mainnet" -> Some Mainnet
    | "custom" -> Some Custom
    | _ -> None
  ;;
end
