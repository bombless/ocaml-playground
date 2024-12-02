
open Option

type color = Red | Black
type 'a element = VirtualLeft | VirtualRight | VirtualNode | VisibleNode of 'a | VisibleLeft | VisibleRight

type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree

let left = Node ((Black, 'B'), Leaf, Leaf)
let right = Node ((Black, 'C'), Leaf, Leaf)
let root = Node ((Red, 'A'), left, right)

let is_leaf = function
  | Leaf -> true
  | _ -> false

let not_leaf x = not @@ is_leaf x

let rec padding = function
  | Leaf -> 0
  | Node (_, Leaf, _) -> 2
  | Node (_, node, _) -> padding node + 2


let rec all_leaf = function
  | [] -> true
  | (h::t) -> is_leaf h && all_leaf t

let rec get_padding_tree (t : 'a tree) carry : (int * 'a) tree = match t with
  | Leaf -> Leaf
  | Node (v, l, r) as x -> Node ((carry + padding x, v), get_padding_tree l carry, get_padding_tree r (carry + 1))

let rec draw_nodes (line : (int * char) list) = match line with
  | [] -> print_char('\n')
  | ((p, c)::t) -> print_string(String.make p ' '); print_char(c); draw_nodes(t);;

let rec generate_first_line (lst : ('a option tree list)) = match lst with
  | [] -> []
  | (Node (Some c, _, _) as x::t) -> (padding x, VisibleNode c) :: generate_first_line t
  | (Node (None, _, _) as x::t) -> (padding x, VirtualNode) :: generate_first_line t
  | (Leaf::t) -> generate_first_line t

let rec generate_next_line (line: (int * 'a element) list) : (int * 'a element) list = match line with
  | [] -> []
  | ((n, c)::t) -> match c with
  | VisibleLeft -> (n - 1, VisibleLeft) :: generate_next_line t
  | VirtualLeft -> (n - 1, VirtualLeft) :: generate_next_line t
  | VisibleRight -> (n + 1, VisibleRight) :: generate_next_line t
  | VirtualRight -> (n + 1, VirtualRight) :: generate_next_line t
  | VisibleNode _ -> (n - 1, VisibleLeft) :: (1, VisibleRight) :: generate_next_line t
  | VirtualNode -> (n - 1, VirtualLeft) :: (1, VirtualRight) :: generate_next_line t
  
let rec generate_lines nodes count_down = match count_down with
  | 0 -> []
  | n -> let new_line = generate_next_line nodes in new_line :: generate_lines new_line (n - 1)

let max a b = if a > b then a else b

let rec get_depth = function
  | Leaf -> 0
  | Node (_, l, r) -> 1 + max (get_depth l) (get_depth r)

let rec as_full_tree t depth = match t with
  | Leaf -> if depth > 0 then Node (none, as_full_tree Leaf (depth - 1), as_full_tree Leaf (depth - 1)) else Leaf
  | Node (v, l, r) -> Node (some v, as_full_tree l (depth - 1), as_full_tree r (depth - 1))

let rec children_of_nodes = function
  | [] -> []
  | (Leaf::t) -> children_of_nodes t
  | (Node (_, l, r)::t) -> l :: r :: children_of_nodes t

let rec list_of_nodes lst depth =
  if depth == 0 then []
  else let children = children_of_nodes lst in
    lst :: list_of_nodes children (depth - 1)

let lines_of_nodes (x: 'a option tree list) depth : (int * 'a element) list list = let first_line = generate_first_line x in first_line :: generate_lines first_line (depth - 1)

let rec concat (lst : 'a list list) : 'a list = match lst with
  | [] -> []
  | (h::t) -> h @ concat t

let get_lines t : (int * 'a element) list list =
  let depth = get_depth t in
  let lists = list_of_nodes [as_full_tree t depth] depth in
  concat @@ List.map (fun x -> lines_of_nodes x depth) lists

let rec print_line = function
  | [] -> print_char '\n'
  | ((n, VisibleNode c)::t) -> print_string (String.make n ' '); print_char c; print_line t
  | ((n, VisibleLeft)::t) -> print_string (String.make n ' '); print_char '/'; print_line t
  | ((n, VisibleRight)::t) -> print_string (String.make n ' '); print_char '\\'; print_line t
  | ((n, _)::t) -> print_string (String.make n ' '); print_char ' '; print_line t
;;

let rec print = function
  | [] -> ()
  | (h::t) -> print_line h; print t;;

let rec clean_tree = function
  | Leaf -> Leaf
  | Node ((_, x), l, r) -> Node (x, clean_tree l, clean_tree r)

let lines = get_lines @@ clean_tree root

let p (_: unit) = print @@ get_lines @@ clean_tree root
