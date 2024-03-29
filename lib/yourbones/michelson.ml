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

module Namespace = struct
  type t =
    | Type
    | Constant
    | Instruction
    | Keyword
    | Constant_hash
end

module Keyword = struct
  type t =
    | Parameter
    | Storage
    | Code
    | View

  let to_string = function
    | Parameter -> "parameter"
    | Storage -> "storage"
    | Code -> "code"
    | View -> "view"
  ;;

  let equal a b =
    match a, b with
    | Parameter, Parameter | Storage, Storage | Code, Code | View, View -> true
    | _ -> false
  ;;
end

module Constant = struct
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

  let to_string = function
    | True -> "True"
    | False -> "False"
    | Elt -> "Elt"
    | Left -> "Left"
    | Right -> "Right"
    | None -> "None"
    | Some -> "Some"
    | Pair -> "Pair"
    | Unit -> "Unit"
    | Lambda_rec -> "Lambda_rec"
  ;;

  let equal a b =
    match a, b with
    | True, True
    | False, False
    | Elt, Elt
    | Left, Left
    | Right, Right
    | None, None
    | Some, Some
    | Pair, Pair
    | Unit, Unit
    | Lambda_rec, Lambda_rec -> true
    | _ -> false
  ;;
end

module Instruction = struct
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

  let to_string = function
    | Pack -> "PACK"
    | Unpack -> "UNPACK"
    | Blake2b -> "BLAKE2B"
    | Sha256 -> "SHA256"
    | Sha512 -> "SHA512"
    | Abs -> "ABS"
    | Add -> "ADD"
    | Amount -> "AMOUNT"
    | And -> "AND"
    | Balance -> "BALANCE"
    | Car -> "CAR"
    | Cdr -> "CDR"
    | Chain_id -> "CHAIN_ID"
    | Check_signature -> "CHECK_SIGNATURE"
    | Compare -> "COMPARE"
    | Concat -> "CONCAT"
    | Cons -> "CONS"
    | Create_account -> "CREATE_ACCOUNT"
    | Create_contract -> "CREATE_CONTRACT"
    | Implicit_account -> "IMPLICIT_ACCOUNT"
    | Dip -> "DIP"
    | Drop -> "DROP"
    | Dup -> "DUP"
    | View -> "VIEW"
    | Ediv -> "EDIV"
    | Empty_big_map -> "EMPTY_BIG_MAP"
    | Empty_map -> "EMPTY_MAP"
    | Empty_set -> "EMPTY_SET"
    | Eq -> "EQ"
    | Exec -> "EXEC"
    | Apply -> "APPLY"
    | Failwith -> "FAILWITH"
    | Ge -> "GE"
    | Get -> "GET"
    | Get_and_update -> "GET_AND_UPDATE"
    | Gt -> "GT"
    | Hash_key -> "HASH_KEY"
    | If -> "IF"
    | If_cons -> "IF_CONS"
    | If_left -> "IF_LEFT"
    | If_none -> "IF_NONE"
    | Int -> "INT"
    | Lambda -> "LAMBDA"
    | Lambda_rec -> "LAMBDA_REC"
    | Le -> "LE"
    | Left -> "LEFT"
    | Level -> "LEVEL"
    | Loop -> "LOOP"
    | Lsl -> "LSL"
    | Lsr -> "LSR"
    | Lt -> "LT"
    | Map -> "MAP"
    | Mem -> "MEM"
    | Mul -> "MUL"
    | Neg -> "NEG"
    | Neq -> "NEQ"
    | Nil -> "NIL"
    | None -> "NONE"
    | Not -> "NOT"
    | Now -> "NOW"
    | Min_block_time -> "MIN_BLOCK_TIME"
    | Or -> "OR"
    | Pair -> "PAIR"
    | Unpair -> "UNPAIR"
    | Push -> "PUSH"
    | Right -> "RIGHT"
    | Size -> "SIZE"
    | Some -> "SOME"
    | Source -> "SOURCE"
    | Sender -> "SENDER"
    | Self -> "SELF"
    | Self_address -> "SELF_ADDRESS"
    | Slice -> "SLICE"
    | Steps_to_quota -> "STEPS_TO_QUOTA"
    | Sub -> "SUB"
    | Sub_mutez -> "SUB_MUTEZ"
    | Swap -> "SWAP"
    | Transfer_tokens -> "TRANSFER_TOKENS"
    | Set_delegate -> "SET_DELEGATE"
    | Unit -> "UNIT"
    | Update -> "UPDATE"
    | Xor -> "XOR"
    | Iter -> "ITER"
    | Loop_left -> "LOOP_LEFT"
    | Address -> "ADDRESS"
    | Contract -> "CONTRACT"
    | Isnat -> "ISNAT"
    | Cast -> "CAST"
    | Rename -> "RENAME"
    | Sapling_empty_state -> "SAPLING_EMPTY_STATE"
    | Sapling_verify_update -> "SAPLING_VERIFY_UPDATE"
    | Dig -> "DIG"
    | Dug -> "DUG"
    | Never -> "NEVER"
    | Voting_power -> "VOTING_POWER"
    | Total_voting_power -> "TOTAL_VOTING_POWER"
    | Keccak -> "KECCAK"
    | Sha3 -> "SHA3"
    | Pairing_check -> "PAIRING_CHECK"
    | Ticket -> "TICKET"
    | Ticket_deprecated -> "TICKET_DEPRECATED"
    | Read_ticket -> "READ_TICKET"
    | Split_ticket -> "SPLIT_TICKET"
    | Join_tickets -> "JOIN_TICKETS"
    | Open_chest -> "OPEN_CHEST"
    | Emit -> "EMIT"
    | Bytes -> "BYTES"
    | Nat -> "NAT"
  ;;

  let equal a b =
    match a, b with
    | Pack, Pack
    | Unpack, Unpack
    | Blake2b, Blake2b
    | Sha256, Sha256
    | Sha512, Sha512
    | Abs, Abs
    | Add, Add
    | Amount, Amount
    | And, And
    | Balance, Balance
    | Car, Car
    | Cdr, Cdr
    | Chain_id, Chain_id
    | Check_signature, Check_signature
    | Compare, Compare
    | Concat, Concat
    | Cons, Cons
    | Create_account, Create_account
    | Create_contract, Create_contract
    | Implicit_account, Implicit_account
    | Dip, Dip
    | Drop, Drop
    | Dup, Dup
    | View, View
    | Ediv, Ediv
    | Empty_big_map, Empty_big_map
    | Empty_map, Empty_map
    | Empty_set, Empty_set
    | Eq, Eq
    | Exec, Exec
    | Apply, Apply
    | Failwith, Failwith
    | Ge, Ge
    | Get, Get
    | Get_and_update, Get_and_update
    | Gt, Gt
    | Hash_key, Hash_key
    | If, If
    | If_cons, If_cons
    | If_left, If_left
    | If_none, If_none
    | Int, Int
    | Lambda, Lambda
    | Lambda_rec, Lambda_rec
    | Le, Le
    | Left, Left
    | Level, Level
    | Loop, Loop
    | Lsl, Lsl
    | Lsr, Lsr
    | Lt, Lt
    | Map, Map
    | Mem, Mem
    | Mul, Mul
    | Neg, Neg
    | Neq, Neq
    | Nil, Nil
    | None, None
    | Not, Not
    | Now, Now
    | Min_block_time, Min_block_time
    | Or, Or
    | Pair, Pair
    | Unpair, Unpair
    | Push, Push
    | Right, Right
    | Size, Size
    | Some, Some
    | Source, Source
    | Sender, Sender
    | Self, Self
    | Self_address, Self_address
    | Slice, Slice
    | Steps_to_quota, Steps_to_quota
    | Sub, Sub
    | Sub_mutez, Sub_mutez
    | Swap, Swap
    | Transfer_tokens, Transfer_tokens
    | Set_delegate, Set_delegate
    | Unit, Unit
    | Update, Update
    | Xor, Xor
    | Iter, Iter
    | Loop_left, Loop_left
    | Address, Address
    | Contract, Contract
    | Isnat, Isnat
    | Cast, Cast
    | Rename, Rename
    | Sapling_empty_state, Sapling_empty_state
    | Sapling_verify_update, Sapling_verify_update
    | Dig, Dig
    | Dug, Dug
    | Never, Never
    | Voting_power, Voting_power
    | Total_voting_power, Total_voting_power
    | Keccak, Keccak
    | Sha3, Sha3
    | Pairing_check, Pairing_check
    | Ticket, Ticket
    | Ticket_deprecated, Ticket_deprecated
    | Read_ticket, Read_ticket
    | Split_ticket, Split_ticket
    | Join_tickets, Join_tickets
    | Open_chest, Open_chest
    | Emit, Emit
    | Bytes, Bytes
    | Nat, Nat -> true
    | _ -> false
  ;;
end

module Type = struct
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

  let to_string = function
    | Bool -> "bool"
    | Contract -> "contract"
    | Int -> "int"
    | Key -> "key"
    | Key_hash -> "key_hash"
    | Lambda -> "lambda"
    | List -> "list"
    | Map -> "map"
    | Big_map -> "big_map"
    | Nat -> "nat"
    | Option -> "option"
    | Or -> "or"
    | Pair -> "pair"
    | Set -> "set"
    | Signature -> "signature"
    | String -> "string"
    | Bytes -> "bytes"
    | Mutez -> "mutez"
    | Timestamp -> "timestamp"
    | Unit -> "unit"
    | Operation -> "operation"
    | Address -> "address"
    | Tx_rollup_l2_address -> "tx_rollup_l2_address"
    | Sapling_state -> "sapling_state"
    | Sapling_transaction -> "sapling_transaction"
    | Sapling_transaction_deprecated -> "sapling_transaction_deprecated"
    | Chain_id -> "chain_id"
    | Never -> "never"
    | Bls12_381_g1 -> "bls12_381_g1"
    | Bls12_381_g2 -> "bls12_381_g2"
    | Bls12_381_fr -> "bls12_381_fr"
    | Ticket -> "ticket"
    | Chest_key -> "chest_key"
    | Chest -> "chest"
  ;;

  let equal a b =
    match a, b with
    | Bool, Bool
    | Contract, Contract
    | Int, Int
    | Key, Key
    | Key_hash, Key_hash
    | Lambda, Lambda
    | List, List
    | Map, Map
    | Big_map, Big_map
    | Nat, Nat
    | Option, Option
    | Or, Or
    | Pair, Pair
    | Set, Set
    | Signature, Signature
    | String, String
    | Bytes, Bytes
    | Mutez, Mutez
    | Timestamp, Timestamp
    | Unit, Unit
    | Operation, Operation
    | Address, Address
    | Tx_rollup_l2_address, Tx_rollup_l2_address
    | Sapling_state, Sapling_state
    | Sapling_transaction, Sapling_transaction
    | Sapling_transaction_deprecated, Sapling_transaction_deprecated
    | Chain_id, Chain_id
    | Never, Never
    | Bls12_381_g1, Bls12_381_g1
    | Bls12_381_g2, Bls12_381_g2
    | Bls12_381_fr, Bls12_381_fr
    | Ticket, Ticket
    | Chest_key, Chest_key
    | Chest, Chest -> true
    | _ -> false
  ;;
end

type t =
  | Keyword of Keyword.t
  | Constant of Constant.t
  | Instruction of Instruction.t
  | Type of Type.t
  | Constant_hash

let k_parameter = Keyword Keyword.Parameter
let k_storage = Keyword Keyword.Storage
let k_code = Keyword Keyword.Code
let k_view = Keyword Keyword.View
let d_true = Constant Constant.True
let d_false = Constant Constant.False
let d_elt = Constant Constant.Elt
let d_left = Constant Constant.Left
let d_right = Constant Constant.Right
let d_none = Constant Constant.None
let d_some = Constant Constant.Some
let d_pair = Constant Constant.Pair
let d_unit = Constant Constant.Unit
let d_lambda_rec = Constant Constant.Lambda_rec
let i_pack = Instruction Instruction.Pack
let i_unpack = Instruction Instruction.Unpack
let i_blake2b = Instruction Instruction.Blake2b
let i_sha256 = Instruction Instruction.Sha256
let i_sha512 = Instruction Instruction.Sha512
let i_abs = Instruction Instruction.Abs
let i_add = Instruction Instruction.Add
let i_amount = Instruction Instruction.Amount
let i_and = Instruction Instruction.And
let i_balance = Instruction Instruction.Balance
let i_car = Instruction Instruction.Car
let i_cdr = Instruction Instruction.Cdr
let i_chain_id = Instruction Instruction.Chain_id
let i_check_signature = Instruction Instruction.Check_signature
let i_compare = Instruction Instruction.Compare
let i_concat = Instruction Instruction.Concat
let i_cons = Instruction Instruction.Cons
let i_create_account = Instruction Instruction.Create_account
let i_create_contract = Instruction Instruction.Create_contract
let i_implicit_account = Instruction Instruction.Implicit_account
let i_dip = Instruction Instruction.Dip
let i_drop = Instruction Instruction.Drop
let i_dup = Instruction Instruction.Dup
let i_view = Instruction Instruction.View
let i_ediv = Instruction Instruction.Ediv
let i_empty_big_map = Instruction Instruction.Empty_big_map
let i_empty_map = Instruction Instruction.Empty_map
let i_empty_set = Instruction Instruction.Empty_set
let i_eq = Instruction Instruction.Eq
let i_exec = Instruction Instruction.Exec
let i_apply = Instruction Instruction.Apply
let i_failwith = Instruction Instruction.Failwith
let i_ge = Instruction Instruction.Ge
let i_get = Instruction Instruction.Get
let i_get_and_update = Instruction Instruction.Get_and_update
let i_gt = Instruction Instruction.Gt
let i_hash_key = Instruction Instruction.Hash_key
let i_if = Instruction Instruction.If
let i_if_cons = Instruction Instruction.If_cons
let i_if_left = Instruction Instruction.If_left
let i_if_none = Instruction Instruction.If_none
let i_int = Instruction Instruction.Int
let i_lambda = Instruction Instruction.Lambda
let i_lambda_rec = Instruction Instruction.Lambda_rec
let i_le = Instruction Instruction.Le
let i_left = Instruction Instruction.Left
let i_level = Instruction Instruction.Level
let i_loop = Instruction Instruction.Loop
let i_lsl = Instruction Instruction.Lsl
let i_lsr = Instruction Instruction.Lsr
let i_lt = Instruction Instruction.Lt
let i_map = Instruction Instruction.Map
let i_mem = Instruction Instruction.Mem
let i_mul = Instruction Instruction.Mul
let i_neg = Instruction Instruction.Neg
let i_neq = Instruction Instruction.Neq
let i_nil = Instruction Instruction.Nil
let i_none = Instruction Instruction.None
let i_not = Instruction Instruction.Not
let i_now = Instruction Instruction.Now
let i_min_block_time = Instruction Instruction.Min_block_time
let i_or = Instruction Instruction.Or
let i_pair = Instruction Instruction.Pair
let i_unpair = Instruction Instruction.Unpair
let i_push = Instruction Instruction.Push
let i_right = Instruction Instruction.Right
let i_size = Instruction Instruction.Size
let i_some = Instruction Instruction.Some
let i_source = Instruction Instruction.Source
let i_sender = Instruction Instruction.Sender
let i_self = Instruction Instruction.Self
let i_self_address = Instruction Instruction.Self_address
let i_slice = Instruction Instruction.Slice
let i_steps_to_quota = Instruction Instruction.Steps_to_quota
let i_sub = Instruction Instruction.Sub
let i_sub_mutez = Instruction Instruction.Sub_mutez
let i_swap = Instruction Instruction.Swap
let i_transfer_tokens = Instruction Instruction.Transfer_tokens
let i_set_delegate = Instruction Instruction.Set_delegate
let i_unit = Instruction Instruction.Unit
let i_update = Instruction Instruction.Update
let i_xor = Instruction Instruction.Xor
let i_iter = Instruction Instruction.Iter
let i_loop_left = Instruction Instruction.Loop_left
let i_address = Instruction Instruction.Address
let i_contract = Instruction Instruction.Contract
let i_isnat = Instruction Instruction.Isnat
let i_cast = Instruction Instruction.Cast
let i_rename = Instruction Instruction.Rename
let i_sapling_empty_state = Instruction Instruction.Sapling_empty_state
let i_sapling_verify_update = Instruction Instruction.Sapling_verify_update
let i_dig = Instruction Instruction.Dig
let i_dug = Instruction Instruction.Dug
let i_never = Instruction Instruction.Never
let i_voting_power = Instruction Instruction.Voting_power
let i_total_voting_power = Instruction Instruction.Total_voting_power
let i_keccak = Instruction Instruction.Keccak
let i_sha3 = Instruction Instruction.Sha3
let i_pairing_check = Instruction Instruction.Pairing_check
let i_ticket = Instruction Instruction.Ticket
let i_ticket_deprecated = Instruction Instruction.Ticket_deprecated
let i_read_ticket = Instruction Instruction.Read_ticket
let i_join_tickets = Instruction Instruction.Join_tickets
let i_split_ticket = Instruction Instruction.Split_ticket
let i_open_chest = Instruction Instruction.Open_chest
let i_emit = Instruction Instruction.Emit
let i_bytes = Instruction Instruction.Bytes
let i_nat = Instruction Instruction.Nat
let t_bool = Type Type.Bool
let t_contract = Type Type.Contract
let t_int = Type Type.Int
let t_key = Type Type.Key
let t_key_hash = Type Type.Key_hash
let t_lambda = Type Type.Lambda
let t_list = Type Type.List
let t_map = Type Type.Map
let t_big_map = Type Type.Big_map
let t_nat = Type Type.Nat
let t_option = Type Type.Option
let t_or = Type Type.Or
let t_pair = Type Type.Pair
let t_set = Type Type.Set
let t_signature = Type Type.Signature
let t_string = Type Type.String
let t_bytes = Type Type.Bytes
let t_mutez = Type Type.Mutez
let t_unit = Type Type.Unit
let t_timestamp = Type Type.Timestamp
let t_operation = Type Type.Operation
let t_address = Type Type.Address
let t_tx_rollup_l2_address = Type Type.Tx_rollup_l2_address
let t_sapling_state = Type Type.Sapling_state
let t_sapling_transaction = Type Type.Sapling_transaction
let t_sapling_transaction_deprecated = Type Type.Sapling_transaction_deprecated
let t_chain_id = Type Type.Chain_id
let t_never = Type Type.Never
let t_bls12_381_g1 = Type Type.Bls12_381_g1
let t_bls12_381_g2 = Type Type.Bls12_381_g2
let t_bls12_381_fr = Type Type.Bls12_381_fr
let t_ticket = Type Type.Ticket
let t_chest_key = Type Type.Chest_key
let t_chest = Type Type.Chest
let h_constant = Constant_hash

let namespace = function
  | Keyword _ -> Namespace.Keyword
  | Constant _ -> Namespace.Constant
  | Instruction _ -> Namespace.Instruction
  | Type _ -> Namespace.Type
  | Constant_hash -> Namespace.Constant_hash
;;

let to_string = function
  | Keyword kwd -> Keyword.to_string kwd
  | Constant cst -> Constant.to_string cst
  | Instruction ins -> Instruction.to_string ins
  | Type typ -> Type.to_string typ
  | Constant_hash -> "constant"
;;

let equal a b =
  match a, b with
  | Keyword a, Keyword b -> Keyword.equal a b
  | Constant a, Constant b -> Constant.equal a b
  | Instruction a, Instruction b -> Instruction.equal a b
  | Type a, Type b -> Type.equal a b
  | Constant_hash, Constant_hash -> true
  | _ -> false
;;

let pp ppf x = Format.fprintf ppf "%s" (to_string x)

let encoding =
  let open Data_encoding in
  (* WARN: According to:
     https://gitlab.com/tezos/tezos/-/blob/master/src/proto_alpha/lib_protocol/michelson_v1_primitives.ml?ref_type=heads#L612
     We should take care of the order.
  *)
  def "michelson.v1.primitives"
  @@ string_enum
       ([ k_parameter
        ; k_storage
        ; k_code
        ; d_false
        ; d_elt
        ; d_left
        ; d_none
        ; d_pair
        ; d_right
        ; d_some
        ; d_true
        ; d_unit
        ; i_pack
        ; i_unpack
        ; i_blake2b
        ; i_sha256
        ; i_sha512
        ; i_abs
        ; i_add
        ; i_amount
        ; i_and
        ; i_balance
        ; i_car
        ; i_cdr
        ; i_check_signature
        ; i_compare
        ; i_concat
        ; i_cons
        ; i_create_account
        ; i_create_contract
        ; i_implicit_account
        ; i_dip
        ; i_drop
        ; i_dup
        ; i_ediv
        ; i_empty_map
        ; i_empty_set
        ; i_eq
        ; i_exec
        ; i_failwith
        ; i_ge
        ; i_get
        ; i_gt
        ; i_hash_key
        ; i_if
        ; i_if_cons
        ; i_if_left
        ; i_if_none
        ; i_int
        ; i_lambda
        ; i_le
        ; i_left
        ; i_loop
        ; i_lsl
        ; i_lsr
        ; i_lt
        ; i_map
        ; i_mem
        ; i_mul
        ; i_neg
        ; i_neq
        ; i_nil
        ; i_none
        ; i_not
        ; i_now
        ; i_or
        ; i_pair
        ; i_push
        ; i_right
        ; i_size
        ; i_some
        ; i_source
        ; i_sender
        ; i_self
        ; i_steps_to_quota
        ; i_sub
        ; i_swap
        ; i_transfer_tokens
        ; i_set_delegate
        ; i_unit
        ; i_update
        ; i_xor
        ; i_iter
        ; i_loop_left
        ; i_address
        ; i_contract
        ; i_isnat
        ; i_cast
        ; i_rename
        ; t_bool
        ; t_contract
        ; t_int
        ; t_key
        ; t_key_hash
        ; t_lambda
        ; t_list
        ; t_map
        ; t_big_map
        ; t_nat
        ; t_option
        ; t_or
        ; t_pair
        ; t_set
        ; t_signature
        ; t_string
        ; t_bytes
        ; t_mutez
        ; t_timestamp
        ; t_unit
        ; t_operation
        ; t_address
        ; i_slice
        ; i_dig
        ; i_dug
        ; i_empty_big_map
        ; i_apply
        ; t_chain_id
        ; i_chain_id
        ; i_level
        ; i_self_address
        ; t_never
        ; i_never
        ; i_unpair
        ; i_voting_power
        ; i_total_voting_power
        ; i_keccak
        ; i_sha3
        ; i_pairing_check
        ; t_bls12_381_g1
        ; t_bls12_381_g2
        ; t_bls12_381_fr
        ; t_sapling_state
        ; t_sapling_transaction_deprecated
        ; i_sapling_empty_state
        ; i_sapling_verify_update
        ; t_ticket
        ; i_ticket_deprecated
        ; i_read_ticket
        ; i_split_ticket
        ; i_join_tickets
        ; i_get_and_update
        ; t_chest
        ; t_chest_key
        ; i_open_chest
        ; i_view
        ; k_view
        ; h_constant
        ; i_sub_mutez
        ; t_tx_rollup_l2_address
        ; i_min_block_time
        ; t_sapling_transaction
        ; i_emit
        ; d_lambda_rec
        ; i_lambda_rec
        ; i_ticket
        ; i_bytes
        ; i_nat
        ]
        |> List.map (fun k -> to_string k, k))
;;
