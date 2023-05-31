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

type t =
  | Acknowledge
  | BroadcastRequest
  | BroadcastResponse
  | Disconnect
  | Error
  | OperationRequest
  | OperationResponse
  | PermissionRequest
  | PermissionResponse
  | SignPayloadRequest
  | SignPayloadResponse

let to_string = function
  | Acknowledge -> "acknowledge"
  | BroadcastRequest -> "broadcast_request"
  | BroadcastResponse -> "broadcast_response"
  | Disconnect -> "disconnect"
  | Error -> "error"
  | OperationRequest -> "operation_request"
  | OperationResponse -> "operation_response"
  | PermissionRequest -> "permission_request"
  | PermissionResponse -> "permission_response"
  | SignPayloadRequest -> "sign_payload_request"
  | SignPayloadResponse -> "sign_payload_response"
;;

let from_string str =
  match Util.normalize str with
  | "acknowledge" -> Some Acknowledge
  | "broadcast_request" -> Some BroadcastRequest
  | "broadcast_response" -> Some BroadcastResponse
  | "disconnect" -> Some Disconnect
  | "error" -> Some Error
  | "operation_request" -> Some OperationRequest
  | "operation_response" -> Some OperationResponse
  | "permission_request" -> Some PermissionRequest
  | "permission_response" -> Some PermissionResponse
  | "sign_payload_request" -> Some SignPayloadRequest
  | "sign_payload_response" -> Some SignPayloadResponse
  | _ -> None
;;
