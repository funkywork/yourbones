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

(** {1 Intermediate representation} *)

type permission_scope = js_string
type network_type = js_string
type color_mode = js_string
type connection_status = js_string
type mutez = js_string
type timeframe = js_string
type origin_kind = js_string

class type network =
  object
    method _type : network_type t readonly_prop
    method name : js_string t or_undefined readonly_prop
    method rpcUrl : js_string t or_undefined readonly_prop
  end

class type threshold =
  object
    method amount : mutez t readonly_prop
    method timeframe : timeframe t readonly_prop
  end

class type permission_entity =
  object
    method address : js_string t readonly_prop
    method network : network t readonly_prop
    method scopes : permission_scope t js_array t readonly_prop
    method threshold : threshold t or_undefined readonly_prop
  end

class type origin =
  object
    method _type : origin_kind t readonly_prop
    method id : js_string t readonly_prop
  end

class type notification =
  object
    method version : int readonly_prop
    method apiUrl : js_string t readonly_prop
    method token : js_string t readonly_prop
  end

class type account_info =
  object
    inherit permission_entity
    method accountIdentifier : js_string t readonly_prop
    method senderId : js_string t readonly_prop
    method origin : origin t readonly_prop
    method walletKey : js_string t or_undefined readonly_prop
    method connectedAt : int readonly_prop
    method notification : notification t or_undefined readonly_prop
  end

class type request_permission_input =
  object
    method network : network t or_undefined readonly_prop
    method scopes : permission_scope t js_array t or_undefined readonly_prop
  end

class type app_metadata =
  object
    method senderId : js_string t readonly_prop
    method name : js_string t readonly_prop
    method icon : js_string t or_undefined readonly_prop
  end

class type beacon_base_message =
  object
    method version : js_string t readonly_prop
    method _id : js_string t readonly_prop
    method senderId : js_string t readonly_prop
  end

class type permission_response =
  object
    inherit beacon_base_message
    method appMetadata : app_metadata t or_undefined readonly_prop
    method publicKey : js_string t readonly_prop
    method network : network t readonly_prop
    method scopes : permission_scope t js_array t readonly_prop
    method threshold : threshold t or_undefined readonly_prop
    method notification : notification t or_undefined readonly_prop
  end

class type permission_response_output =
  object
    inherit permission_response
    method address : js_string t readonly_prop
    method accountInfo : account_info t readonly_prop
    method walletKey : js_string t or_undefined readonly_prop
  end

class type request_broadcast_input =
  object
    method network : network t or_undefined readonly_prop
    method signedTransaction : js_string t readonly_prop
  end

class type broadcast_response =
  object
    method _type : js_string t readonly_prop
    method transactionHash : js_string t readonly_prop
  end

class type broadcast_response_output = broadcast_response

(** {1 DAppClient}

    Bindings for
    {{:https://typedocs.walletbeacon.io/classes/dappclient.html} DAppClient} *)

class type dapp_client_options =
  object
    method name : js_string t readonly_prop
    method appUrl : js_string t or_undefined readonly_prop
    method colorMode : color_mode t or_undefined readonly_prop
    method disableDefaultEvents : bool t or_undefined readonly_prop
    method disclaimerText : js_string t or_undefined readonly_prop
    method iconUrl : js_string t or_undefined readonly_prop
    method matrixNodes : js_string t js_array t or_undefined readonly_prop
    method description : js_string t or_undefined readonly_prop
    method preferredNetwork : network_type t or_undefined readonly_prop
    method featuredWallets : js_string t js_array t or_undefined readonly_prop

    (* FIXME: Unsupported features. *)
    (* method eventHandlers : event_handlers t or_undefined readonly_prop *)
    (* method blockExplorer : block_explorer t or_undefined readonly_prop *)
    (* method storage : storage t or_undefined readonly_prop *)
  end

class type dapp_client =
  object
    inherit dapp_client_options
    method beaconId : js_string t Promise.t readonly_prop
    method connectionStatus : connection_status t Promise.t readonly_prop
    method ready : unit Promise.t readonly_prop
    method checkPermissions : js_string t -> bool t Promise.t meth
    method clearActiveAccount : unit Promise.t meth
    method destroy : unit Promise.t meth

    method getAccount :
      js_string t -> account_info t or_undefined Promise.t meth

    method getAccounts : account_info t js_array t Promise.t meth
    method getActiveAccount : account_info t or_undefined Promise.t meth

    method requestBroadcast :
      request_broadcast_input t -> broadcast_response_output t Promise.t meth

    method requestPermissions :
      request_permission_input t or_undefined
      -> permission_response_output t Promise.t meth
  end
