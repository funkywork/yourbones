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

(** Dapp_client allows interaction with Beacon to orchestrate a wallet from a
    web browser. It is a functional wrapper on top of :
    {{:https://typedocs.walletbeacon.io/classes/dappclient.html#constructor}
      DAppCLient}. *)

(** {1 Types} *)

(** An handler for a DAppClient. *)
type t

(** {1 Instanciate a Client} *)

(** [make ~name ()] builds a DApp client. You can use optional arguments in
    order to give more information about the application.

    {{:https://typedocs.walletbeacon.io/interfaces/dappclientoptions.html} See
      for more information}. *)
val make
  :  ?app_url:string
  -> ?color_mode:Color_mode.t
  -> ?disable_default_event:bool
  -> ?disclaimer_text:string
  -> ?icon_url:string
  -> ?matrix_nodes:string list
  -> ?preferred_network:Yourbones_common.network_type
  -> ?description:string
  -> ?featured_wallets:string list
  -> name:string
  -> unit
  -> t

(** {1 Properties} *)

(** [beacon_id client] returns the identifier wrapped into a promise of the
    beacon session for a given [client].*)
val beacon_id : t -> string Lwt.t

(** [connection_status client] returns the connection status wrapped into a
    promise for a given [client]. *)
val connection_status : t -> Transport_status.t Lwt.t

(** [ready client] returns a promise that awaits that the client is ready. *)
val ready : t -> unit Lwt.t

(** [check_permissions client message] returns a promise that check if we have
    permissions to send the specific message type to the active account. If no
    active account is set, only permission requests are allowed.. *)
val check_permissions : t -> Message_type.t -> bool Lwt.t

(** [clear_active_account client] returns a promise that clear the active
    account.*)
val clear_active_account : t -> unit Lwt.t

(** [destroy client] returns a promise that erases all client information. *)
val destroy : t -> unit Lwt.t

(** [get_account client account_identifier] returns a promise that try to reach
    an account by the [account_identifier]. *)
val get_account : t -> string -> Account_info.t option Lwt.t

(** [get_accounts client] returns a promise that get the list of all locally
    knowns accounts. *)
val get_accounts : t -> Account_info.t list Lwt.t

(** [get_active_account client] returns a promise that get the current active
    account. *)
val get_active_account : t -> Account_info.t option Lwt.t

(** [request_permissions ?network ?scopes client] send a permission request to
    the DApp. This should be done as the first step. The wallet will respond
    with an publicKey and permissions that were given. The account returned will
    be set as the "activeAccount" and will be used for the following requests.*)
val request_permissions
  :  ?network:Network.t
  -> ?scopes:Permission_scope.t list
  -> t
  -> Permission_response_output.t Lwt.t
