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
open Yourbones_common

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

    val preferredNetwork =
      Js.string % Network.Type.to_string <$> preferred_network |> to_optdef

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
