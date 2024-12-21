(*
 * to play it in utop:
 * $ ocamlc -c tree.ml
 * $ utop
 * utop # #load "tree.cmo";;
 * utop # #use "pretty_tree.ml";;
 *)

open Tree

let rec height_of_depth = function
  | n when n <= 1 -> 0
  | 2 -> 2
  | 3 -> 3
  | n -> 1 + 2 * height_of_depth (n - 1)

let rec leftmost_space_of_depth = function
  | n when n <= 1 -> 0
  | 2 -> 2
  | n -> leftmost_space_of_depth (n - 1) + height_of_depth n + 1

let normal_space_of_depth n = leftmost_space_of_depth (n + 1) - 1

let rec get_padding_tree (t : 'a tree) depth is_left is_leftmost : (int * 'a) tree = match t with
  | Leaf -> Leaf
  | Node (v, l, r) ->
    let padding =
      if is_leftmost
      then leftmost_space_of_depth depth
      else if is_left && depth == 1 then 1 else normal_space_of_depth depth in
    Node ((padding, v), get_padding_tree l (depth - 1) true is_leftmost, get_padding_tree r (depth - 1) false false)

let rec generate_first_line (lst : ((int * 'a option) tree list)) = match lst with
  | [] -> []
  | (Node ((p, Some c), l, r)::t) -> (p, VisibleNode (c, is_visual_leaf l, is_visual_leaf r)) :: generate_first_line t
  | (Node ((p, None), _, _)::t) -> (p, VirtualNode) :: generate_first_line t
  | (Leaf::t) -> generate_first_line t

let rec generate_next_line (line: (int * 'a element) list) first : (int * 'a element) list =
  let offset = if first then 1 else 2 in
  match line with
    | [] -> []
    | ((n, c)::t) -> match c with
      | VisibleLeft -> (n - offset, VisibleLeft) :: generate_next_line t false
      | VirtualLeft -> (n - offset, VirtualLeft) :: generate_next_line t false
      | VisibleRight -> (n + offset, VisibleRight) :: generate_next_line t false
      | VirtualRight -> (n + offset, VirtualRight) :: generate_next_line t false
      | VisibleNode (_, left_is_leaf, right_is_leaf) -> (n, if left_is_leaf then VirtualLeft else VisibleLeft) :: (1, if right_is_leaf then VirtualRight else VisibleRight) :: generate_next_line t false
      | VirtualNode -> (n - offset, VirtualLeft) :: (1, VirtualRight) :: generate_next_line t false
  
let rec generate_lines nodes count_down = match count_down with
  | 0 -> []
  | n -> let new_line = generate_next_line nodes true in new_line :: generate_lines new_line (n - 1)

let rec as_full_tree t depth = match t with
  | Leaf -> if depth > 0 then Node (Option.none, as_full_tree Leaf (depth - 1), as_full_tree Leaf (depth - 1)) else Leaf
  | Node (v, l, r) -> Node (Option.some v, as_full_tree l (depth - 1), as_full_tree r (depth - 1))

let rec children_of_nodes = function
  | [] -> []
  | (Leaf::t) -> children_of_nodes t
  | (Node (_, l, r)::t) -> l :: r :: children_of_nodes t

let rec list_of_nodes lst depth =
  if depth == 0 then []
  else let children = children_of_nodes lst in
    lst :: list_of_nodes children (depth - 1)

let lines_of_nodes (x: (int * 'a option) tree list) : (int * 'a element) list list = match x with
  | (item::_) ->
    let depth = get_depth item in
    let count_down = height_of_depth depth in
    let first_line = generate_first_line x in
    first_line :: generate_lines first_line count_down
  | _ -> []
  

let rec concat (lst : 'a list list) : 'a list = match lst with
  | [] -> []
  | (h::t) -> h @ concat t

let get_lines t : (int * 'a element) list list =
  let depth = get_depth t in
  let lists = list_of_nodes [get_padding_tree (as_full_tree t depth) depth true true] depth in
  concat @@ List.map lines_of_nodes lists

module Printer (Tree : sig
  type t
  val print_node : t -> unit
end) =
struct
  let rec print_line = function
    | [] -> print_char '\n'
    | ((n, VisibleNode (c, _, _))::t) -> print_string (String.init n (fun _ -> ' ')); Tree.print_node c; print_line t
    | ((n, VisibleLeft)::t) -> print_string (String.make n ' '); print_char '/'; print_line t
    | ((n, VisibleRight)::t) -> print_string (String.make n ' '); print_char '\\'; print_line t
    | ((n, VirtualNode)::t) -> print_string (String.make (n + 3) ' '); print_line t
    | ((n, _)::t) -> print_string (String.make n ' '); print_char ' '; print_line t
  ;;
  let rec print_helper = function
    | [] -> ()
    | (h::t) -> print_line h; print_helper t;;
  let print x = print_helper @@ get_lines x
end

module CharTree = Printer (struct
  type t = char
  let print_node = Printf.printf "|%c|"
end);;

let e = Node ('A', Node ('B', Leaf, Leaf), Leaf);;
CharTree.print e;;
CharTree.print Char.(insert 'D' @@ insert 'A' @@ insert 'F' @@ insert 'H' @@ insert 'E' Leaf);;

open Random

let generate_random_uppercase_chars n =
  Random.self_init ();
  List.init n (fun _ ->
    let ascii_code = Random.int 26 + 65 in
    char_of_int ascii_code)

let () =
  let random_chars = generate_random_uppercase_chars 16 in
  let rec fold f acc = function
  | [] -> acc
  | h::t -> f h (fold f acc t) in
  fold Char.insert Leaf random_chars |> CharTree.print
