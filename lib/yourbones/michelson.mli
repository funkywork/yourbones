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

(** Description of Michelson primitives, the low-level smart-contract language
    of the Tezos chain. *)

(** {1 Types}

    The logic for linking primitives is provided by {!mod:Micheline}, allowing
    Michelson expressions to be extended without modifying the existing code
    structure.

    The various primitives are described in several compartments, known as
    {!module:Namespace}:

    - {!type:keyword} which describes the primitives used to describe a
      contract.
    - {!type:constant} which describes the constants primitives. *)

(** Describes the different namespaces (primitives organisations). *)
module Namespace : sig
  (** All namespaces. *)
  type t =
    | Type
    | Constant
    | Instruction
    | Keyword
    | Constant_hash
end

(** Describes the keywords Michelson Primitives. *)
module Keyword : sig
  (** Keywords primitives. *)
  type t =
    | Parameter
    | Storage
    | Code
    | View

  val to_string : t -> string
end

(** Describes the Constant Michelson Primitives. *)
module Constant : sig
  (** Constants primitives. *)
  type t =
    | True
    | False
    | Elt
    | Left
    | Right
    | None
    | Some
    | Pair
    | Unit
    | Lambda_rec

  val to_string : t -> string
end

(** Describes the Constant Michelson Primitives. *)
module Instruction : sig
  (** Instruction primitives. *)
  type t =
    | Pack
    | Unpack
    | Blake2b
    | Sha256
    | Sha512
    | Abs
    | Add
    | Amount
    | And
    | Balance
    | Car
    | Cdr
    | Chain_id
    | Check_signature
    | Compare
    | Concat
    | Cons
    | Create_account
    | Create_contract
    | Implicit_account
    | Dip
    | Drop
    | Dup
    | View
    | Ediv
    | Empty_big_map
    | Empty_map
    | Empty_set
    | Eq
    | Exec
    | Apply
    | Failwith
    | Ge
    | Get
    | Get_and_update
    | Gt
    | Hash_key
    | If
    | If_cons
    | If_left
    | If_none
    | Int
    | Lambda
    | Lambda_rec
    | Le
    | Left
    | Level
    | Loop
    | Lsl
    | Lsr
    | Lt
    | Map
    | Mem
    | Mul
    | Neg
    | Neq
    | Nil
    | None
    | Not
    | Now
    | Min_block_time
    | Or
    | Pair
    | Unpair
    | Push
    | Right
    | Size
    | Some
    | Source
    | Sender
    | Self
    | Self_address
    | Slice
    | Steps_to_quota
    | Sub
    | Sub_mutez
    | Swap
    | Transfer_tokens
    | Set_delegate
    | Unit
    | Update
    | Xor
    | Iter
    | Loop_left
    | Address
    | Contract
    | Isnat
    | Cast
    | Rename
    | Sapling_empty_state
    | Sapling_verify_update
    | Dig
    | Dug
    | Never
    | Voting_power
    | Total_voting_power
    | Keccak
    | Sha3
    | Pairing_check
    | Ticket
    | Ticket_deprecated
    | Read_ticket
    | Split_ticket
    | Join_tickets
    | Open_chest
    | Emit
    | Bytes
    | Nat

  val to_string : t -> string
end

(** Describes the Type Michelson Primitives. *)
module Type : sig
  (** Type primitives. *)
  type t =
    | Bool
    | Contract
    | Int
    | Key
    | Key_hash
    | Lambda
    | List
    | Map
    | Big_map
    | Nat
    | Option
    | Or
    | Pair
    | Set
    | Signature
    | String
    | Bytes
    | Mutez
    | Timestamp
    | Unit
    | Operation
    | Address
    | Tx_rollup_l2_address
    | Sapling_state
    | Sapling_transaction
    | Sapling_transaction_deprecated
    | Chain_id
    | Never
    | Bls12_381_g1
    | Bls12_381_g2
    | Bls12_381_fr
    | Ticket
    | Chest_key
    | Chest

  val to_string : t -> string
end

(** The full set of Michelson primitives. *)
type prim =
  | Keyword of Keyword.t
  | Constant of Constant.t
  | Instruction of Instruction.t
  | Type of Type.t
  | Constant_hash

(** {1 Building primitives}

    Helpers for building primitives. Function are prefixed by a letter to avoid
    name clashes. *)

(** {2 Keyword primitives} *)

val k_parameter : prim
val k_storage : prim
val k_code : prim
val k_view : prim

(** {2 Constant primitives} *)

val d_true : prim
val d_false : prim
val d_elt : prim
val d_left : prim
val d_right : prim
val d_none : prim
val d_some : prim
val d_pair : prim
val d_unit : prim
val d_lambda_rec : prim

(** {2 Instruction primitives} *)

val i_pack : prim
val i_unpack : prim
val i_blake2b : prim
val i_sha256 : prim
val i_sha512 : prim
val i_abs : prim
val i_add : prim
val i_amount : prim
val i_and : prim
val i_balance : prim
val i_car : prim
val i_cdr : prim
val i_chain_id : prim
val i_check_signature : prim
val i_compare : prim
val i_concat : prim
val i_cons : prim
val i_create_account : prim
val i_create_contract : prim
val i_implicit_account : prim
val i_dip : prim
val i_drop : prim
val i_dup : prim
val i_view : prim
val i_ediv : prim
val i_empty_big_map : prim
val i_empty_map : prim
val i_empty_set : prim
val i_eq : prim
val i_exec : prim
val i_apply : prim
val i_failwith : prim
val i_ge : prim
val i_get : prim
val i_get_and_update : prim
val i_gt : prim
val i_hash_key : prim
val i_if : prim
val i_if_cons : prim
val i_if_left : prim
val i_if_none : prim
val i_int : prim
val i_lambda : prim
val i_lambda_rec : prim
val i_le : prim
val i_left : prim
val i_level : prim
val i_loop : prim
val i_lsl : prim
val i_lsr : prim
val i_lt : prim
val i_map : prim
val i_mem : prim
val i_mul : prim
val i_neg : prim
val i_neq : prim
val i_nil : prim
val i_none : prim
val i_not : prim
val i_now : prim
val i_min_block_time : prim
val i_or : prim
val i_pair : prim
val i_unpair : prim
val i_push : prim
val i_right : prim
val i_size : prim
val i_some : prim
val i_source : prim
val i_sender : prim
val i_self : prim
val i_self_address : prim
val i_slice : prim
val i_steps_to_quota : prim
val i_sub : prim
val i_sub_mutez : prim
val i_swap : prim
val i_transfer_tokens : prim
val i_set_delegate : prim
val i_unit : prim
val i_update : prim
val i_xor : prim
val i_iter : prim
val i_loop_left : prim
val i_address : prim
val i_contract : prim
val i_isnat : prim
val i_cast : prim
val i_rename : prim
val i_sapling_empty_state : prim
val i_sapling_verify_update : prim
val i_dig : prim
val i_dug : prim
val i_never : prim
val i_voting_power : prim
val i_total_voting_power : prim
val i_keccak : prim
val i_sha3 : prim
val i_pairing_check : prim
val i_ticket : prim
val i_ticket_deprecated : prim
val i_read_ticket : prim
val i_split_ticket : prim
val i_join_tickets : prim
val i_open_chest : prim
val i_emit : prim
val i_bytes : prim
val i_nat : prim

(** {2 Types primitives} *)

val t_bool : prim
val t_contract : prim
val t_int : prim
val t_key : prim
val t_key_hash : prim
val t_lambda : prim
val t_list : prim
val t_map : prim
val t_big_map : prim
val t_nat : prim
val t_option : prim
val t_or : prim
val t_pair : prim
val t_set : prim
val t_signature : prim
val t_string : prim
val t_bytes : prim
val t_mutez : prim
val t_timestamp : prim
val t_unit : prim
val t_operation : prim
val t_address : prim
val t_tx_rollup_l2_address : prim
val t_sapling_state : prim
val t_sapling_transaction : prim
val t_sapling_transaction_deprecated : prim
val t_chain_id : prim
val t_never : prim
val t_bls12_381_g1 : prim
val t_bls12_381_g2 : prim
val t_bls12_381_fr : prim
val t_ticket : prim
val t_chest_key : prim
val t_chest : prim

(** {2 Constant Hash primitive} *)

val h_constant : prim

(** {1 Additional features} *)

(** [namespace prim] returns the corresponding namespace from a {!type:prim}. *)
val namespace : prim -> Namespace.t

(** [prim_to_string prim] returns a prim as a string. *)
val prim_to_string : prim -> string
