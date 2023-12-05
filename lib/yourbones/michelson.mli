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
  val equal : t -> t -> bool
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
  val equal : t -> t -> bool
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
  val equal : t -> t -> bool
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
  val equal : t -> t -> bool
end

(** The full set of Michelson primitives. *)
type t =
  | Keyword of Keyword.t
  | Constant of Constant.t
  | Instruction of Instruction.t
  | Type of Type.t
  | Constant_hash

(** {1 Building primitives}

    Helpers for building primitives. Function are prefixed by a letter to avoid
    name clashes. *)

(** {2 Keyword primitives} *)

val k_parameter : t
val k_storage : t
val k_code : t
val k_view : t

(** {2 Constant primitives} *)

val d_true : t
val d_false : t
val d_elt : t
val d_left : t
val d_right : t
val d_none : t
val d_some : t
val d_pair : t
val d_unit : t
val d_lambda_rec : t

(** {2 Instruction primitives} *)

val i_pack : t
val i_unpack : t
val i_blake2b : t
val i_sha256 : t
val i_sha512 : t
val i_abs : t
val i_add : t
val i_amount : t
val i_and : t
val i_balance : t
val i_car : t
val i_cdr : t
val i_chain_id : t
val i_check_signature : t
val i_compare : t
val i_concat : t
val i_cons : t
val i_create_account : t
val i_create_contract : t
val i_implicit_account : t
val i_dip : t
val i_drop : t
val i_dup : t
val i_view : t
val i_ediv : t
val i_empty_big_map : t
val i_empty_map : t
val i_empty_set : t
val i_eq : t
val i_exec : t
val i_apply : t
val i_failwith : t
val i_ge : t
val i_get : t
val i_get_and_update : t
val i_gt : t
val i_hash_key : t
val i_if : t
val i_if_cons : t
val i_if_left : t
val i_if_none : t
val i_int : t
val i_lambda : t
val i_lambda_rec : t
val i_le : t
val i_left : t
val i_level : t
val i_loop : t
val i_lsl : t
val i_lsr : t
val i_lt : t
val i_map : t
val i_mem : t
val i_mul : t
val i_neg : t
val i_neq : t
val i_nil : t
val i_none : t
val i_not : t
val i_now : t
val i_min_block_time : t
val i_or : t
val i_pair : t
val i_unpair : t
val i_push : t
val i_right : t
val i_size : t
val i_some : t
val i_source : t
val i_sender : t
val i_self : t
val i_self_address : t
val i_slice : t
val i_steps_to_quota : t
val i_sub : t
val i_sub_mutez : t
val i_swap : t
val i_transfer_tokens : t
val i_set_delegate : t
val i_unit : t
val i_update : t
val i_xor : t
val i_iter : t
val i_loop_left : t
val i_address : t
val i_contract : t
val i_isnat : t
val i_cast : t
val i_rename : t
val i_sapling_empty_state : t
val i_sapling_verify_update : t
val i_dig : t
val i_dug : t
val i_never : t
val i_voting_power : t
val i_total_voting_power : t
val i_keccak : t
val i_sha3 : t
val i_pairing_check : t
val i_ticket : t
val i_ticket_deprecated : t
val i_read_ticket : t
val i_split_ticket : t
val i_join_tickets : t
val i_open_chest : t
val i_emit : t
val i_bytes : t
val i_nat : t

(** {2 Types primitives} *)

val t_bool : t
val t_contract : t
val t_int : t
val t_key : t
val t_key_hash : t
val t_lambda : t
val t_list : t
val t_map : t
val t_big_map : t
val t_nat : t
val t_option : t
val t_or : t
val t_pair : t
val t_set : t
val t_signature : t
val t_string : t
val t_bytes : t
val t_mutez : t
val t_timestamp : t
val t_unit : t
val t_operation : t
val t_address : t
val t_tx_rollup_l2_address : t
val t_sapling_state : t
val t_sapling_transaction : t
val t_sapling_transaction_deprecated : t
val t_chain_id : t
val t_never : t
val t_bls12_381_g1 : t
val t_bls12_381_g2 : t
val t_bls12_381_fr : t
val t_ticket : t
val t_chest_key : t
val t_chest : t

(** {2 Constant Hash primitive} *)

val h_constant : t

(** {1 Additional features} *)

(** [namespace prim] returns the corresponding namespace from a {!type:t}. *)
val namespace : t -> Namespace.t

(** [prim_to_string prim] returns a prim as a string. *)
val to_string : t -> string

(** Equality between {!type:t}. *)
val equal : t -> t -> bool

(** Pretty printer for {!type:t}. *)
val pp : Format.formatter -> t -> unit

(** Data encoding for {!type:t}. *)
val encoding : t Data_encoding.t
