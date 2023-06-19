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

let destruct_with_result encoding value =
  try
    let value = Data_encoding.Json.destruct encoding value in
    Ok value
  with
  | exn -> Error (`Json_exn exn)
;;

let call
  ?parameters
  ?headers
  ?body
  ?mode
  ?credentials
  ?cache
  ?redirect
  ?referrer
  ?referrer_policy
  ?integrity
  ?keepalive
  ~node_address
  entrypoint
  =
  let entrypoint = entrypoint ~node_address in
  let endpoint = Yourbones.RPC.endpoint_of entrypoint in
  let method_ = Yourbones.RPC.method_of entrypoint in
  let encoding = Yourbones.RPC.encoding_of entrypoint in
  Nightmare_service.Endpoint.gen_link ?parameters endpoint (fun target ->
    let open Nightmare_js in
    let open Lwt.Syntax in
    let* response =
      Fetch.fetch
        ~method_
        ?headers
        ?body
        ?mode
        ?credentials
        ?cache
        ?redirect
        ?referrer
        ?referrer_policy
        ?integrity
        ?keepalive
        target
    in
    if Fetch.Response.is_ok response
    then
      let+ txt = Fetch.Response.text response in
      txt
      |> Data_encoding.Json.from_string
      |> Result.map_error (fun message -> `Json_error message)
      |> fun json -> Result.bind json @@ destruct_with_result encoding
    else Lwt.return (Error (`Http_error (Fetch.Response.status response))))
;;

let ( let*? ) promise f =
  Lwt.bind promise (function
    | Ok x -> f x
    | Error err -> Lwt.return_error err)
;;

let stream
  ?(retention_policy = `Restart_after 1.0)
  ?parameters
  ?headers
  ?body
  ?mode
  ?credentials
  ?cache
  ?redirect
  ?referrer
  ?referrer_policy
  ?integrity
  ?keepalive
  ~on_chunk
  ~node_address
  entrypoint
  =
  let entrypoint = entrypoint ~node_address in
  let endpoint = Yourbones.RPC.endpoint_of entrypoint in
  let method_ = Yourbones.RPC.method_of entrypoint in
  let encoding = Yourbones.RPC.encoding_of entrypoint in
  Nightmare_service.Endpoint.gen_link ?parameters endpoint (fun target ->
    let rec loop () =
      let open Nightmare_js in
      let open Lwt.Syntax in
      let* response =
        Fetch.fetch
          ~method_
          ?headers
          ?body
          ?mode
          ?credentials
          ?cache
          ?redirect
          ?referrer
          ?referrer_policy
          ?integrity
          ?keepalive
          target
      in
      if Fetch.Response.is_ok response
      then (
        let body = Fetch.Response.body response in
        let reader = Stream.Readable.get_reader body in
        let rec read () =
          let* is_done, txt = Stream.Reader.read_string reader in
          if is_done
          then Lwt.return_ok ()
          else
            let*? obj =
              txt
              |> Data_encoding.Json.from_string
              |> Result.map_error (fun message -> `Json_error message)
              |> fun json ->
              Result.bind json @@ destruct_with_result encoding |> Lwt.return
            in
            let*? () =
              let* chunk = on_chunk obj in
              match chunk with
              | Ok x -> Lwt.return_ok x
              | Error err -> Lwt.return_error (`On_chunk err)
            in
            read ()
        in
        Lwt.catch read (fun exn ->
          let () =
            Nightmare_js.Console.(string warning)
              "Trapped exception in RPC.stream"
          in
          let () = Nightmare_js.Console.error exn in
          match retention_policy with
          | `Restart_after delay ->
            let () =
              Nightmare_js.Console.(string warning)
              @@ "Restarting after "
              ^ string_of_float delay
            in
            let* () = Js_of_ocaml_lwt.Lwt_js.sleep delay in
            loop ()
          | `Raise handler -> handler exn))
      else Lwt.return (Error (`Http_error (Fetch.Response.status response)))
    in
    loop ())
;;
