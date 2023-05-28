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

(** A partial binding of {{:ttps://docs.walletbeacon.io/} BeaconWallet}. This
    module exposes the internal plumbing and should not be used outside this
    library. *)

open Js_of_ocaml
open Js
open Nightmare_js

(** {1 DAppClient}

    Bindings for
    {{:https://typedocs.walletbeacon.io/classes/dappclient.html} DAppClient} *)

class type dapp_client_options =
  object
    method appUrl : js_string t or_undefined readonly_prop
    method colorMode : js_string t or_undefined readonly_prop
    method disableDefaultEvents : bool t or_undefined readonly_prop
    method disclaimerText : js_string t or_undefined readonly_prop
    method iconUrl : js_string t or_undefined readonly_prop
    method matrixNodes : js_string t js_array t or_undefined readonly_prop
    method name : js_string t readonly_prop
    method preferredNetwork : js_string t or_undefined readonly_prop

    (* FIXME: Unsupported features. *)
    (* method eventHandlers : event_handlers t or_undefined readonly_prop *)
    (* method blockExplorer : block_explorer t or_undefined readonly_prop *)
    (* method storage : storage t or_undefined readonly_prop *)
  end

class type dapp_client =
  object
    inherit dapp_client_options
    method beaconId : js_string t Promise.t readonly_prop
    method connectionStatus : js_string t Promise.t readonly_prop
    method ready : unit Promise.t readonly_prop
  end
