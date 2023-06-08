open Js_of_ocaml
open Beacon
open Js_of_ocaml_lwt

let (Network.{ type_ = preferred_network; _ } as network) = Network.ghostnet ()

let mount client account =
  let app = Dom_html.getElementById "app" in
  let message =
    "Connected with "
    ^ Yourbones.Address.to_string account.Account_info.address
    ^ "<br /><button id='send-tez'>Press me</button>"
  in
  let () = app##.innerHTML := Js.string message in
  let btn = Dom_html.getElementById "send-tez" in
  Lwt_js_events.(async_loop click) btn (fun _ _ ->
    let open Lwt.Syntax in
    let+ Transaction_hash_response_output.{ transaction_hash; _ } =
      Dapp_client.request_simple_transaction
        ~destination:[%address "tz1XxRjkB77R1rnxVRGQxmWAcG1cGQQeMAAL"]
        client
        1t
    in
    Nightmare_js.Console.(string log) transaction_hash)
;;

let _ =
  let open Lwt.Syntax in
  let client = Dapp_client.make ~preferred_network ~name:"Test" () in
  let* account = Dapp_client.get_active_account client in
  match account with
  | None ->
    let btn = Dom_html.getElementById "request-perm" in
    Lwt_js_events.(async_loop click) btn (fun _ _ ->
      let* Permission_response_output.{ account_info; _ } =
        Dapp_client.request_permissions ~network client
      in
      mount client account_info)
  | Some account -> mount client account
;;
