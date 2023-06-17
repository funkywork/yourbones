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
      |> Result.map @@ Data_encoding.Json.destruct encoding
      |> Result.map_error (fun message -> `Json_error message)
    else Lwt.return (Error (`Http_error (Fetch.Response.status response))))
;;
