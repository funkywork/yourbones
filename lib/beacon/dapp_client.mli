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

(** [ready client] a promise that awaits that the client is ready. *)
val ready : t -> unit Lwt.t
