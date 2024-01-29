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

(** Calls RPCs described in the [Yourbones.RPC] framework and automates decoding
    via [Data_encoding]. *)

(** [call ~node_address entrypoint] call an entrypoint of the Tezos Node. *)
val call
  :  ?parameters:(string * string) list
  -> ?headers:Nightmare_js.Headers.t
  -> ?body:Nightmare_js.Fetch.body
  -> ?mode:Nightmare_js.Fetch.mode
  -> ?credentials:Nightmare_js.Fetch.credentials
  -> ?cache:Nightmare_js.Fetch.cache
  -> ?redirect:Nightmare_js.Fetch.redirect
  -> ?referrer:Nightmare_js.Fetch.referrer
  -> ?referrer_policy:Nightmare_js.Fetch.referrer_policy
  -> ?integrity:string
  -> ?keepalive:bool
  -> node_address:string
  -> ( Nightmare_service.Method.t
       , 'encoding
       , 'continuation
       , ( 'encoding
           , [> `Json_error of string | `Json_exn of exn | `Http_error of int ]
           )
           result
           Lwt.t )
       Yourbones.RPC.wrapped
  -> 'continuation

(** [stream ~on_chunk ~node_address ~entrypoint] stream an entrypoint (ie: head
    monitoring) and perform [on_chunk] on each new bunch of data. *)
val stream
  :  ?retention_policy:
       [< `Restart_after of float
       | `Raise of
         exn
         -> ( unit
              , ([> `Json_error of string
                 | `Json_exn of exn
                 | `Http_error of int
                 | `On_chunk of 'new_errors
                 ]
                 as
                 'errors) )
              result
              Lwt.t > `Restart_after
       ]
  -> ?parameters:(string * string) list
  -> ?headers:Nightmare_js.Headers.t
  -> ?body:Nightmare_js.Fetch.body
  -> ?mode:Nightmare_js.Fetch.mode
  -> ?credentials:Nightmare_js.Fetch.credentials
  -> ?cache:Nightmare_js.Fetch.cache
  -> ?redirect:Nightmare_js.Fetch.redirect
  -> ?referrer:Nightmare_js.Fetch.referrer
  -> ?referrer_policy:Nightmare_js.Fetch.referrer_policy
  -> ?integrity:string
  -> ?keepalive:bool
  -> on_chunk:('encoding -> (unit, 'new_errors) result Lwt.t)
  -> node_address:string
  -> ( Nightmare_service.Method.t
       , 'encoding
       , 'continuation
       , (unit, 'errors) result Lwt.t )
       Yourbones.RPC.wrapped
  -> 'continuation
