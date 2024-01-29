open Js_of_ocaml
open Beacon
open Js_of_ocaml_lwt

let (Network.{ type_ = preferred_network; _ } as network) = Network.ghostnet ()
let node_address = "https://ghostnet.ecadinfra.com"

let mount client account =
  let open Lwt.Syntax in
  let* tez =
    Yourbones_js.RPC.call
      ~node_address
      Yourbones.RPC.Directory.get_balance
      Yourbones.Chain_id.main
      Yourbones.Block_id.head
      account.Account_info.address
  in
  let tez = Result.fold ~ok:(fun x -> x) ~error:(fun _ -> 0t) tez in
  let app = Dom_html.getElementById "app" in
  let message =
    "Connected with "
    ^ Yourbones.Address.to_string account.Account_info.address
    ^ " ("
    ^ Format.asprintf "%a" Yourbones.Tez.pp_print tez
    ^ ")"
    ^ "<br /><button id='send-tez'>Press me</button>"
  in
  let () = app##.innerHTML := Js.string message in
  let btn = Dom_html.getElementById "send-tez" in
  let* _ =
    let container = Dom_html.getElementById "block-header" in
    Yourbones_js.RPC.stream
      ~on_chunk:(fun header ->
        let () = Nightmare_js.Console.(string log) "new block" in
        let hash = header.Yourbones.Block_header.hash in
        let value = Yourbones.Block_hash.to_string hash in
        let () = container##.innerHTML := Js.string value in
        Lwt.return_ok ())
      ~node_address
      Yourbones.RPC.Directory.monitor_heads
      Yourbones.Chain_id.main
  in
  Lwt_js_events.(async_loop click) btn (fun _ _ ->
    let+ result =
      let () = Nightmare_js.Console.(string log) "clicked" in
      Dapp_client.request_simple_transaction
        ~destination:[%address "tz1XxRjkB77R1rnxVRGQxmWAcG1cGQQeMAAL"]
        client
        1t
    in
    match result with
    | Ok Transaction_hash_response_output.{ transaction_hash; _ } ->
      Nightmare_js.Console.(string log) transaction_hash
    | Error (`Request_operation_rejection _) ->
      Nightmare_js.Console.(string error) "Request_operation_rejection")
;;

let _ =
  let open Lwt.Syntax in
  let client = Dapp_client.make ~preferred_network ~name:"Test" () in
  let* account = Dapp_client.get_active_account client in
  match account with
  | None ->
    let btn = Dom_html.getElementById "request-perm" in
    Lwt_js_events.(async_loop click) btn (fun _ _ ->
      let* result = Dapp_client.request_permissions ~network client in
      match result with
      | Ok Permission_response_output.{ account_info; _ } ->
        mount client account_info
      | Error (`Request_permissions_rejection _) ->
        let () =
          Nightmare_js.Console.(string error) "Request_permissions_rejection"
        in
        Lwt.return_unit)
  | Some account -> mount client account
;;
