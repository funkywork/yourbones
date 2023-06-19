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

(** the [Yourbones] module exposes all backend agnostic tools (e.g. as a node,
    to build an indexer or JavaScript to build the front-end of a dApp).

    It is mainly used to describe data to interact with the chain. *)

(** {1 Common types}

    Exposes all recurring types that are often used (such as [tez]). *)

type tez = Tez.t
type network_type = Network.Type.t

(** {1 Tezos related modules} *)

module Tez = Tez
module Address = Address
module Block_hash = Block_hash
module Chain_id = Chain_id
module Block_id = Block_id

(** {1 Node related modules} *)

module Network = Network
module RPC = Rpc
