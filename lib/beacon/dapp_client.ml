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

(** A concrete implementation of DAppClient. *)

open Js_of_ocaml
open Nightmare_js

type t = Bindings.dapp_client Js.t
type constr = (Bindings.dapp_client_options Js.t -> t) Js.constr

let make_options
  ?app_url
  ?color_mode
  ?disable_default_event
  ?disclaimer_text
  ?icon_url
  ?matrix_nodes
  ?preferred_network
  ?description
  ?featured_wallets
  ~name
  ()
  : Bindings.dapp_client_options Js.t
  =
  let open Preface.Fun.Infix in
  let open Option in
  object%js
    val appUrl = Js.string <$> app_url |> to_optdef
    val colorMode = Color_mode.as_optional_parameter color_mode
    val disableDefaultEvents = Js.bool <$> disable_default_event |> to_optdef
    val disclaimerText = Js.string <$> disclaimer_text |> to_optdef
    val iconUrl = Js.string <$> icon_url |> to_optdef
    val name = Js.string name
    val description = Js.string <$> description |> to_optdef

    val featuredWallets =
      Util.list_to_js_with Js.string <$> featured_wallets |> to_optdef

    val preferredNetwork =
      Js.string % Yourbones.Network.Type.to_string
      <$> preferred_network
      |> to_optdef

    val matrixNodes =
      Util.list_to_js_with Js.string <$> matrix_nodes |> to_optdef
  end
;;

let make
  ?app_url
  ?color_mode
  ?disable_default_event
  ?disclaimer_text
  ?icon_url
  ?matrix_nodes
  ?preferred_network
  ?description
  ?featured_wallets
  ~name
  ()
  =
  let options =
    make_options
      ?app_url
      ?color_mode
      ?disable_default_event
      ?disclaimer_text
      ?icon_url
      ?matrix_nodes
      ?preferred_network
      ?description
      ?featured_wallets
      ~name
      ()
  in
  let constr : constr = Js.Unsafe.global##._DAppClient in
  new%js constr options
;;

let beacon_id client =
  let open Lwt.Syntax in
  let+ id = client##.beaconId |> Promise.as_lwt in
  Js.to_string id
;;

let connection_status client =
  let open Lwt.Syntax in
  let+ status = client##.connectionStatus |> Promise.as_lwt in
  status
  |> Js.to_string
  |> Transport_status.from_string
  |> Option.value ~default:Transport_status.Not_connected
;;

let ready client = client##.ready |> Promise.as_lwt

let check_permissions client message =
  let open Lwt.Syntax in
  let message_str = message |> Message_type.to_string |> Js.string in
  let+ result = client##checkPermissions message_str |> Promise.as_lwt in
  Js.to_bool result
;;

let clear_active_account client = client##clearActiveAccount |> Promise.as_lwt
let destroy client = client##destroy |> Promise.as_lwt

let to_account_info potential_account =
  let open Undefinable in
  Account_info.from_js <$> potential_account |> to_option
;;

let get_account client identifier =
  let open Lwt.Syntax in
  let identifier_str = Js.string identifier in
  let+ account = client##getAccount identifier_str |> Promise.as_lwt in
  to_account_info account
;;

let get_accounts client =
  let open Lwt.Syntax in
  let+ accounts = client##getAccounts |> Promise.as_lwt in
  Util.list_from_js_with Account_info.from_js accounts
;;

let get_active_account client =
  let open Lwt.Syntax in
  let+ account = client##getActiveAccount |> Promise.as_lwt in
  to_account_info account
;;

let request_permissions ?network ?scopes client =
  let input : Bindings.request_permission_input Js.t or_undefined =
    match network, scopes with
    | None, None -> Undefinable.empty
    | _ ->
      let open Option in
      let open Preface.Fun.Infix in
      Undefinable.fill
        (object%js
           val network = Network.to_js <$> network |> to_optdef

           val scopes =
             Util.list_to_js_with (Js.string % Permission_scope.to_string)
             <$> scopes
             |> to_optdef
        end)
  in
  let wrapped () =
    let open Lwt.Syntax in
    let+ output = client##requestPermissions input |> Promise.as_lwt in
    Ok (Permission_response_output.from_js output)
  and handler exn = Lwt.return @@ Error (`Request_permissions_rejection exn) in
  Lwt.catch wrapped handler
;;

let request_broadcast ?network ~signed_transaction client =
  let input =
    Request_broadcast_input.({ network; signed_transaction } |> to_js)
  in
  let wrapped () =
    let open Lwt.Syntax in
    let+ output = client##requestBroadcast input |> Promise.as_lwt in
    Ok (Transaction_hash_response_output.from_js output)
  and handler exn = Lwt.return @@ Error (`Request_broadcast_rejection exn) in
  Lwt.catch wrapped handler
;;

let request_simple_transaction
  ?source
  ?fee
  ?counter
  ?gas_limit
  ?storage_limit
  ~destination
  client
  amount
  =
  let batch =
    [ Operation.Transaction.forge
        ?source
        ?fee
        ?counter
        ?gas_limit
        ?storage_limit
        ~destination
        amount
    ]
    |> Operation.batch
  in
  let wrapped () =
    let open Lwt.Syntax in
    let+ output = client##requestOperation batch |> Promise.as_lwt in
    Ok (Transaction_hash_response_output.from_js output)
  and handler exn = Lwt.return @@ Error (`Request_operation_rejection exn) in
  Lwt.catch wrapped handler
;;
