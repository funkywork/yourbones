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

type ('continuation, 'witness) path =
  ('continuation, 'witness) Nightmare_service.path

type ('method_, 'encoding, 'continuation, 'witness) entrypoint =
  { encoding : 'encoding Data_encoding.t
  ; endpoint :
      ( [ `Outer ]
        , 'method_
        , 'continuation
        , 'witness )
        Nightmare_service.wrapped_endpoint
  }

type ('method_, 'encoding, 'continuation, 'witness) wrapped =
  node_address:string
  -> ('method_, 'encoding, 'continuation, 'witness) entrypoint

module Infix = struct
  let ( ~/ ) constant = Nightmare_service.Path.(add_constant constant root)
  let ( ~/: ) hole = Nightmare_service.Path.(add_variable hole root)
  let ( / ) p constant = Nightmare_service.Path.(add_constant constant p)
  let ( /: ) p hole = Nightmare_service.Path.(add_variable hole p)
  let ( ~: ) p = p ()
end

include Infix

let get ~encoding path ~node_address =
  let endpoint () = Nightmare_service.Endpoint.(outer get node_address path) in
  { encoding; endpoint }
;;

let post ~encoding path ~node_address =
  let endpoint () = Nightmare_service.Endpoint.(outer post node_address path) in
  { encoding; endpoint }
;;

let put ~encoding path ~node_address =
  let endpoint () = Nightmare_service.Endpoint.(outer put node_address path) in
  { encoding; endpoint }
;;

let delete ~encoding path ~node_address =
  let endpoint () =
    Nightmare_service.Endpoint.(outer delete node_address path)
  in
  { encoding; endpoint }
;;

let head ~encoding path ~node_address =
  let endpoint () = Nightmare_service.Endpoint.(outer head node_address path) in
  { encoding; endpoint }
;;

let connect ~encoding path ~node_address =
  let endpoint () =
    Nightmare_service.Endpoint.(outer connect node_address path)
  in
  { encoding; endpoint }
;;

let options ~encoding path ~node_address =
  let endpoint () =
    Nightmare_service.Endpoint.(outer options node_address path)
  in
  { encoding; endpoint }
;;

let trace ~encoding path ~node_address =
  let endpoint () =
    Nightmare_service.Endpoint.(outer trace node_address path)
  in
  { encoding; endpoint }
;;

let patch ~encoding path ~node_address =
  let endpoint () =
    Nightmare_service.Endpoint.(outer patch node_address path)
  in
  { encoding; endpoint }
;;

let endpoint_of { endpoint; _ } = endpoint

let method_of entrypoint =
  let endpoint = endpoint_of entrypoint in
  Nightmare_service.Endpoint.method_of endpoint
;;

let encoding_of { encoding; _ } = encoding

module Directory = struct
  module Internal = struct
    let chain_by_id () = ~/"chains" /: Chain_id.fragment
    let block_by_id () = ~:chain_by_id / "blocks" /: Block_id.fragment
    let context () = ~:block_by_id / "context"
    let contract () = ~:context / "contracts" /: Address.fragment
    let balance () = ~:contract / "balance"
    let monitor_heads () = ~/"monitor" / "heads" /: Chain_id.fragment
  end

  let get_balance ~node_address =
    get ~encoding:Tez.encoding ~node_address ~:Internal.balance
  ;;

  let monitor_heads ~node_address =
    get ~encoding:Block_header.encoding ~node_address ~:Internal.monitor_heads
  ;;
end
