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

(** A collection of tools for building entry points for a node's RPC server. The
    aim is essentially to read the blockchain; to interact, it's best to use a
    wallet (or the Beacon API). *)

(** {1 Types} *)

(** A shortcut that describes a path. *)
type ('continuation, 'witness) path =
  ('continuation, 'witness) Nightmare_service.path

(** an entrypoint is similar to an endpoint (in the Nightmare sense) except that
    it is always [outer] and also embeds an encoding, to enable automatic
    decoding of the information. *)
type ('method_, 'encoding, 'continuation, 'witness) entrypoint

(** A [wrapped] entrypoint is wrapped in a [unit -> entrypoint] function to
    avoid the value restriction. *)
type ('method_, 'encoding, 'continuation, 'witness) wrapped =
  unit -> ('method_, 'encoding, 'continuation, 'witness) entrypoint

(** {1 Entrypoint construction} *)

val get
  :  encoding:'encoding Data_encoding.t
  -> ('continuation, 'witness) path
  -> node_address:string
  -> ([> `GET ], 'encoding, 'continuation, 'witness) entrypoint

val post
  :  encoding:'encoding Data_encoding.t
  -> ('continuation, 'witness) path
  -> node_address:string
  -> ([> `POST ], 'encoding, 'continuation, 'witness) entrypoint

val put
  :  encoding:'encoding Data_encoding.t
  -> ('continuation, 'witness) path
  -> node_address:string
  -> ([> `PUT ], 'encoding, 'continuation, 'witness) entrypoint

val delete
  :  encoding:'encoding Data_encoding.t
  -> ('continuation, 'witness) path
  -> node_address:string
  -> ([> `DELETE ], 'encoding, 'continuation, 'witness) entrypoint

val head
  :  encoding:'encoding Data_encoding.t
  -> ('continuation, 'witness) path
  -> node_address:string
  -> ([> `HEAD ], 'encoding, 'continuation, 'witness) entrypoint

val connect
  :  encoding:'encoding Data_encoding.t
  -> ('continuation, 'witness) path
  -> node_address:string
  -> ([> `CONNECT ], 'encoding, 'continuation, 'witness) entrypoint

val options
  :  encoding:'encoding Data_encoding.t
  -> ('continuation, 'witness) path
  -> node_address:string
  -> ([> `OPTIONS ], 'encoding, 'continuation, 'witness) entrypoint

val trace
  :  encoding:'encoding Data_encoding.t
  -> ('continuation, 'witness) path
  -> node_address:string
  -> ([> `TRACE ], 'encoding, 'continuation, 'witness) entrypoint

val patch
  :  encoding:'encoding Data_encoding.t
  -> ('continuation, 'witness) path
  -> node_address:string
  -> ([> `PATCH ], 'encoding, 'continuation, 'witness) entrypoint

(** {1 Retreive data about entrypoints} *)

val method_of
  :  (Nightmare_service.Method.t, 'encoding, 'continuation, 'witness) wrapped
  -> [> Nightmare_service.Method.t ]

val encoding_of
  :  (Nightmare_service.Method.t, 'encoding, 'continuation, 'witness) wrapped
  -> 'encoding Data_encoding.t

val endpoint_of
  :  (Nightmare_service.Method.t, 'encoding, 'continuation, 'witness) wrapped
  -> ( [ `Outer ]
     , Nightmare_service.Method.t
     , 'continuation
     , 'witness )
     Nightmare_service.wrapped_endpoint

(** {1 Infix operators}

    Infix operators for easy path construction. *)

module Infix : sig
  val ( ~/ ) : string -> ('witness, 'witness) path

  val ( ~/: )
    :  'new_variable Nightmare_service.Path.variable
    -> ('new_variable -> 'witness, 'witness) path

  val ( / )
    :  ('continuation, 'witness) path
    -> string
    -> ('continuation, 'witness) path

  val ( /: )
    :  ('continuation, 'new_variable -> 'witness) path
    -> 'new_variable Nightmare_service.Path.variable
    -> ('continuation, 'witness) path
end

include module type of Infix

(** {1 Directory}

    Although each dApp can build its own set of RPCs using the combiners
    described above, here's a list of pre-implemented RPCs that may prove useful
    in dApp development, on a recurring basis. *)

module Directory : sig
  (** [get_balance ~node_address] describes the entrypoint
      [/chains/<chain_id>/blocks/<block_id>/context/contracts/<contract_id>/balance (GET)] *)
  val get_balance
    :  node_address:string
    -> ( [> `GET ]
       , Tez.t
       , Chain_id.t -> Block_id.t -> Address.t -> 'witness
       , 'witness )
       wrapped
end
