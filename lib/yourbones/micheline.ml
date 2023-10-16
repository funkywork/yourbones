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
end
