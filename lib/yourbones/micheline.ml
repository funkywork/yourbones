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

type ('location, 'primitive) t =
  | Int of 'location * Z.t
  | String of 'location * string
  | Bytes of 'location * bytes
  | Prim of 'location * 'primitive * ('location, 'primitive) t list * annotation
  | Seq of 'location * ('location, 'primitive) t list

and annotation = string list

let z ~location x = Int (location, x)
let int ~location x = Int (location, Z.of_int x)
let int64 ~location x = Int (location, Z.of_int64 x)
let string ~location x = String (location, x)
let bytes ~location x = Bytes (location, x)
let seq ~location nodes = Seq (location, nodes)

let prim ?(annotation = []) ~location primitive nodes =
  Prim (location, primitive, nodes, annotation)
;;

let annotation = function
  | Prim (_, _, _, x) -> x
  | _ -> []
;;

let location = function
  | Int (loc, _)
  | String (loc, _)
  | Bytes (loc, _)
  | Prim (loc, _, _, _)
  | Seq (loc, _) -> loc
;;

module Continue = struct
  type ('location, 'primitive) n = ('location, 'primitive) t

  type ('location, 'primitive, 'location_xs, 'primitive_xs) t =
    | For_seq of
        'location_xs * ('location, 'primitive, 'location_xs, 'primitive_xs) l
    | For_prim of
        'location_xs
        * 'primitive_xs
        * annotation
        * ('location, 'primitive, 'location_xs, 'primitive_xs) l

  and ('location, 'primitive, 'location_xs, 'primitive_xs) l =
    | Done
    | For_list of
        ('location, 'primitive) n list
        * ('location_xs, 'primitive_xs) n list
        * ('location, 'primitive, 'location_xs, 'primitive_xs) t
end

let pp ?pp_location ~pp_primitive () ppf expr =
  let pp_location =
    Option.fold
      ~none:(fun ppf _ -> Format.fprintf ppf "")
      ~some:(fun pp ppf expr -> Format.fprintf ppf "%a, " pp expr)
      pp_location
  in
  let rec aux ppf = function
    | Int (loc, value) ->
      Format.fprintf ppf "Int(%a%a)" pp_location loc Z.pp_print value
    | String (loc, value) ->
      Format.fprintf ppf "String(%a\"%s\")" pp_location loc value
    | Bytes (loc, value) ->
      Format.fprintf
        ppf
        "Bytes(%a%a)"
        pp_location
        loc
        Format.pp_print_bytes
        value
    | Seq (loc, xs) ->
      Format.fprintf
        ppf
        "Seq[%a%a]"
        pp_location
        loc
        (Format.pp_print_list aux)
        xs
    | Prim (loc, prim, xs, annot) ->
      Format.fprintf
        ppf
        "Prim(%a%a[%a][%a])"
        pp_location
        loc
        pp_primitive
        prim
        Format.(pp_print_list pp_print_string)
        annot
        (Format.pp_print_list aux)
        xs
  in
  aux ppf expr
;;

let equal loc_equal prim_equal =
  let rec equal left right =
    match left, right with
    | Int (a, x), Int (b, y) -> loc_equal a b && Z.equal x y
    | String (a, x), String (b, y) -> loc_equal a b && String.equal x y
    | Bytes (a, x), Bytes (b, y) -> loc_equal a b && Bytes.equal x y
    | Seq (a, x), Seq (b, y) -> loc_equal a b && List.equal equal x y
    | Prim (a, a_prim, a_xs, a_annot), Prim (b, b_prim, b_xs, b_annot) ->
      loc_equal a b
      && prim_equal a_prim b_prim
      && List.equal equal a_xs b_xs
      && List.equal String.equal a_annot b_annot
    | _ -> false
  in
  equal
;;

module Canonical = struct
  type location = int
  type nonrec 'primitive t = Canonical of (location, 'primitive) t [@@unboxed]

  let dummy_location = -1
  let z = z ~location:dummy_location
  let int = int ~location:dummy_location
  let int64 = int64 ~location:dummy_location
  let string = string ~location:dummy_location
  let bytes = bytes ~location:dummy_location
  let seq x = seq ~location:dummy_location x
  let prim ?annotation = prim ?annotation ~location:dummy_location
  let to_node (Canonical node) = node

  let from_node node =
    let current_loc = ref (-1) in
    let get_loc () =
      let () = incr current_loc in
      !current_loc
    in
    let rec aux node cont =
      let loc = get_loc () in
      match node with
      | Int (_previous_loc, value) -> run (Int (loc, value)) cont
      | String (_previous_loc, value) -> run (String (loc, value)) cont
      | Bytes (_previous_loc, value) -> run (Bytes (loc, value)) cont
      | Seq (_previous_loc, sequence) ->
        aux_list [] (Continue.For_seq (loc, cont)) sequence
      | Prim (_, prim, sequence, annot) ->
        aux_list [] (Continue.For_prim (loc, prim, annot, cont)) sequence
    and aux_list acc cont = function
      | [] -> run_list (List.rev acc) cont
      | x :: xs -> aux x (Continue.For_list (xs, acc, cont))
    and run node = function
      | Continue.For_list (xs, acc, cont) -> aux_list (node :: acc) cont xs
      | Continue.Done -> node
    and run_list nodes = function
      | Continue.For_seq (loc, cont) -> run (Seq (loc, nodes)) cont
      | Continue.For_prim (loc, prim, annot, cont) ->
        run (Prim (loc, prim, nodes, annot)) cont
    in
    let result = aux node Continue.Done in
    Canonical result
  ;;

  let annotation x = x |> to_node |> annotation
  let location x = x |> to_node |> location

  let equal prim_equal (Canonical a) (Canonical b) =
    equal Int.equal prim_equal a b
  ;;

  let pp pp_primitive ppf (Canonical expr) =
    Format.fprintf ppf "%a" (pp ~pp_primitive ()) expr
  ;;
end

let canonical_encoding prim_encoding =
  let open Data_encoding in
  let int_encoding tag =
    case
      tag
      (obj1 (req "int" z))
      ~title:"Int"
      (function
        | Int (_, x) -> Some x
        | _ -> None)
      (fun x -> Int (0, x))
  in
  let string_encoding tag =
    case
      tag
      (obj1 (req "string" string))
      ~title:"String"
      (function
        | String (_, x) -> Some x
        | _ -> None)
      (fun x -> String (0, x))
  in
  let bytes_encoding tag =
    case
      tag
      (obj1 (req "bytes" bytes))
      ~title:"bytes"
      (function
        | Bytes (_, x) -> Some x
        | _ -> None)
      (fun x -> Bytes (0, x))
  in
  let seq_encoding tag fixpoint =
    case
      tag
      (list fixpoint)
      ~title:"Sequence"
      (function
        | Seq (_, x) -> Some x
        | _ -> None)
      (fun x -> Seq (0, x))
  in
  let annots_encoding =
    (* FIXME: Some checks should be added. *)
    splitted
      ~json:(list (Bounded.string 255))
      ~binary:(conv (String.concat " ") (String.split_on_char ' ') string)
  in
  let application_encoding tag fixpoint =
    case
      tag
      ~title:"Prim__generic"
      (obj3
         (req "prim" prim_encoding)
         (dft "args" (list fixpoint) [])
         (dft "annots" annots_encoding []))
      (function
        | Prim (_, prim, args, annots) -> Some (prim, args, annots)
        | _ -> None)
      (fun (prim, args, annots) -> Prim (0, prim, args, annots))
  in
  let node_encoding =
    mu "micheline.expression" (fun fixpoint ->
      splitted
        ~json:
          (union
             ~tag_size:`Uint8
             [ int_encoding Json_only
             ; string_encoding Json_only
             ; bytes_encoding Json_only
             ; seq_encoding Json_only fixpoint
             ; application_encoding Json_only fixpoint
             ])
        ~binary:
          (union
             ~tag_size:`Uint8
             [ (* Simple cases *)
               int_encoding (Tag 0)
             ; string_encoding (Tag 1)
             ; seq_encoding (Tag 2) fixpoint
             ; (* Specific cases with optim *)
               case
                 (Tag 3)
                 ~title:"Prim__no_args__no_annots"
                 (obj1 (req "prim" prim_encoding))
                 (function
                   | Prim (_, x, [], []) -> Some x
                   | _ -> None)
                 (fun x -> Prim (0, x, [], []))
             ; case
                 (Tag 4)
                 ~title:"Prim__no_args__some_annots"
                 (obj2
                    (req "prim" prim_encoding)
                    (req "annots" annots_encoding))
                 (function
                   | Prim (_, x, [], annots) -> Some (x, annots)
                   | _ -> None)
                 (fun (x, annots) -> Prim (0, x, [], annots))
             ; case
                 (Tag 5)
                 ~title:"Prim__1_args__no_annots"
                 (obj2 (req "prim" prim_encoding) (req "arg" fixpoint))
                 (function
                   | Prim (_, x, [ arg ], []) -> Some (x, arg)
                   | _ -> None)
                 (fun (x, arg) -> Prim (0, x, [ arg ], []))
             ; case
                 (Tag 6)
                 ~title:"Prim__1_args__some_annots"
                 (obj3
                    (req "prim" prim_encoding)
                    (req "arg" fixpoint)
                    (req "annots" annots_encoding))
                 (function
                   | Prim (_, x, [ arg ], annots) -> Some (x, arg, annots)
                   | _ -> None)
                 (fun (x, arg, annots) -> Prim (0, x, [ arg ], annots))
             ; case
                 (Tag 7)
                 ~title:"Prim__2_args__no_annots"
                 (obj3
                    (req "prim" prim_encoding)
                    (req "arg1" fixpoint)
                    (req "arg2" fixpoint))
                 (function
                   | Prim (_, x, [ arg1; arg2 ], []) -> Some (x, arg1, arg2)
                   | _ -> None)
                 (fun (x, arg1, arg2) -> Prim (0, x, [ arg1; arg2 ], []))
             ; case
                 (Tag 8)
                 ~title:"Prim__2_args__some_annots"
                 (obj4
                    (req "prim" prim_encoding)
                    (req "arg1" fixpoint)
                    (req "arg2" fixpoint)
                    (req "annots" annots_encoding))
                 (function
                   | Prim (_, x, [ arg1; arg2 ], annots) ->
                     Some (x, arg1, arg2, annots)
                   | _ -> None)
                 (fun (x, arg1, arg2, annots) ->
                   Prim (0, x, [ arg1; arg2 ], annots))
               (* General cases *)
             ; application_encoding (Tag 9) fixpoint
             ; bytes_encoding (Tag 10)
             ]))
  in
  conv Canonical.to_node Canonical.from_node node_encoding
;;

let encoding prim_encoding =
  Data_encoding.conv
    Canonical.from_node
    Canonical.to_node
    (canonical_encoding prim_encoding)
;;
