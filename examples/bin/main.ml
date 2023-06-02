open Js_of_ocaml
open Beacon
open Js_of_ocaml_lwt

let mount account =
  let app = Dom_html.getElementById "app" in
  let message =
    "Connected with " ^ Yourbones.Address.to_string account.Account_info.address
  in
  let () = app##.innerHTML := Js.string message in
  Lwt.return_unit
;;

let _ =
  let open Lwt.Syntax in
  let client = Dapp_client.make ~name:"Test" () in
  let* account = Dapp_client.get_active_account client in
  match account with
  | None ->
    let btn = Dom_html.getElementById "request-perm" in
    Lwt_js_events.(async_loop click) btn (fun _ _ ->
      let* Permission_response_output.{ account_info; _ } =
        Dapp_client.request_permissions client
      in
      mount account_info)
  | Some account -> mount account
;;
