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

type t =
  { hash : Block_hash.t
  ; level : int
  ; proto : int
  ; predecessor : Block_hash.t
  ; validation_passes : int
  ; timestamp : string (* TODO: Fix the timestamp representation *)
  ; fitness : string list (* TODO: Fix fitness representation *)
  ; context : string (* TODO: Fix the context_hash representation *)
  ; operations_hash : string
      (* TODO: Fix operation hash list list representation*)
  ; protocol_data : string (* FIXME: fix protocol data representation*)
  }

let inject
  { hash
  ; level
  ; proto
  ; predecessor
  ; validation_passes
  ; timestamp
  ; fitness
  ; context
  ; operations_hash
  ; protocol_data
  }
  =
  ( hash
  , level
  , proto
  , predecessor
  , validation_passes
  , timestamp
  , fitness
  , context
  , operations_hash
  , protocol_data )
;;

let project
  ( hash
  , level
  , proto
  , predecessor
  , validation_passes
  , timestamp
  , fitness
  , context
  , operations_hash
  , protocol_data )
  =
  { hash
  ; level
  ; proto
  ; predecessor
  ; validation_passes
  ; timestamp
  ; fitness
  ; context
  ; operations_hash
  ; protocol_data
  }
;;

let encoding =
  let open Data_encoding in
  conv
    inject
    project
    (obj10
       (req "hash" Block_hash.encoding)
       (req "level" int31)
       (req "proto" int8)
       (req "predecessor" Block_hash.encoding)
       (req "validation_pass" int8)
       (req "timestamp" string)
       (req "fitness" (list ?max_length:None string))
       (req "context" string)
       (req "operations_hash" string)
       (req "protocol_data" string))
;;
