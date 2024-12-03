
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

let max a b = if a > b then a else b

let rec get_depth = function
  | Leaf -> 0
  | Node (_, l, r) -> 1 + max (get_depth l) (get_depth r)

let rec leftmost_leaf_nodes_padding_of_depth = function
  | 0 -> 0
  | 1 -> 0
  | 2 -> 1
  | x -> 2 * leftmost_leaf_nodes_padding_of_depth (x - 1)

let rec leftmost_three_spaces_padding_of_depth = function
  | 0 -> 0
  | 1 -> 0
  | 2 -> 1
  | 3 -> 3
  | x -> 2 * leftmost_three_spaces_padding_of_depth (x - 1)

let rec leftmost_one_space_padding_of_depth = function
  | 0 -> 0
  | 1 -> 0
  | 2 -> 0
  | 3 -> 0
  | 4 -> 1
  | x -> 2 * leftmost_one_space_padding_of_depth (x - 1)

let leftmost_padding_of_depth n = leftmost_leaf_nodes_padding_of_depth n + leftmost_three_spaces_padding_of_depth n + leftmost_one_space_padding_of_depth n

(* n >= 1 *)
let normal_node_padding_of_depth n = if n < 2 then 3 else leftmost_padding_of_depth (n + 1)

let rec get_padding_tree (t : 'a tree) depth is_left is_leftmost : (int * 'a) tree = match t with
  | Leaf -> Leaf
  | Node (v, l, r) as x ->
    let padding =
      if is_leftmost
      then leftmost_padding_of_depth depth
      else if is_left && depth == 1 then 1 else normal_node_padding_of_depth depth in
    Node ((padding, v), get_padding_tree l (depth - 1) true is_leftmost, get_padding_tree r (depth - 1) false false)


let rec generate_first_line (lst : ((int * 'a option) tree list)) = match lst with
  | [] -> []
  | (Node ((p, Some c), _, _) as x::t) -> (p, VisibleNode c) :: generate_first_line t
  | (Node ((p, None), _, _) as x::t) -> (p, VirtualNode) :: generate_first_line t
  | (Leaf::t) -> generate_first_line t

let rec generate_next_line (line: (int * 'a element) list) first : (int * 'a element) list =
  let offset = if first then 1 else 2 in
  match line with
    | [] -> []
    | ((n, c)::t) -> match c with
    | VisibleLeft -> (n - 1, VisibleLeft) :: generate_next_line t false
    | VirtualLeft -> (n - 1, VirtualLeft) :: generate_next_line t false
    | VisibleRight -> (n + 2, VisibleRight) :: generate_next_line t false
    | VirtualRight -> (n + 1, VirtualRight) :: generate_next_line t false
    | VisibleNode _ -> (n - offset, VisibleLeft) :: (1, VisibleRight) :: generate_next_line t false
    | VirtualNode -> (n - offset, VirtualLeft) :: (1, VirtualRight) :: generate_next_line t false
  
let rec generate_lines nodes count_down = match count_down with
  | 0 -> []
  | n -> let new_line = generate_next_line nodes true in new_line :: generate_lines new_line (n - 1)

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

let lines_of_nodes (x: (int * 'a option) tree list) : (int * 'a element) list list = match x with
  | (item::_) -> let first_line = generate_first_line x in first_line :: generate_lines first_line (get_depth item - 1)
  | _ -> []
  

let rec concat (lst : 'a list list) : 'a list = match lst with
  | [] -> []
  | (h::t) -> h @ concat t

let get_lines t : (int * 'a element) list list =
  let depth = get_depth t in
  let lists = list_of_nodes [get_padding_tree (as_full_tree t depth) depth true true] depth in
  concat @@ List.map lines_of_nodes lists

module type Data = sig
  type t

  val dataset: int -> t tree

  val put: t -> unit

  val print: t tree -> unit
end


module Print(Data: Data) = struct
  let rec print_line = function
    | [] -> print_char '\n'
    | ((n, VisibleNode c)::t) -> print_string (String.make n ' '); Data.put c; print_line t
    | ((n, VisibleLeft)::t) -> print_string (String.make n ' '); print_char '/'; print_line t
    | ((n, VisibleRight)::t) -> print_string (String.make n ' '); print_char '\\'; print_line t
    | ((n, _)::t) -> print_string (String.make n ' '); print_char ' '; print_line t
  ;;
  let rec print_helper = function
    | [] -> ()
    | (h::t) -> print_line h; print_helper t;;
  let print x = print_helper @@ get_lines x
end


module rec RedBlackData : Data = struct
  type t = color * char
  let dataset = fun i -> [|root|].(i)
  let red_text = "\027[31m"
  let green_text = "\027[32m"
  let reset_color = "\027[0m"
  let put = function
    | (Red, c) -> Printf.printf "%s%c%s" red_text c reset_color
    | (_, c) -> print_char(c)
  include Print(RedBlackData)
end


let rec clean_tree = function
  | Leaf -> Leaf
  | Node ((_, x), l, r) -> Node (x, clean_tree l, clean_tree r)

let big_tree =
  let a = Node ('A', Leaf, Leaf) in
  let c = Node ('C', Leaf, Leaf) in
  let e = Node ('E', Leaf, Leaf) in
  let g = Node ('G', Leaf, Leaf) in
  let b = Node ('B', a, c) in
  let f = Node ('F', e, g) in
  let d = Node ('D', b, f) in
  d

let leaf c = Node (c, Leaf, Leaf)
let node c l r = Node (c, l, r)

let big_big_tree =
  let n0 = leaf '0' in
  let n2 = leaf '2' in
  let n4 = leaf '4' in
  let n6 = leaf '6' in
  let n8 = leaf '8' in
  let na = leaf 'A' in
  let nc = leaf 'C' in
  let ne = leaf 'E' in
  let ng = leaf 'G' in
  let ni = leaf 'I' in
  let nk = leaf 'K' in
  let nm = leaf 'M' in
  let no = leaf 'O' in
  let nq = leaf 'Q' in
  let ns = leaf 'S' in
  let nu = leaf 'U' in
  let n1 = node '1' n0 n2 in
  let n5 = node '5' n4 n6 in
  let n9 = node '9' n8 na in
  let nd = node 'D' nc ne in
  let n3 = node '3' n1 n5 in
  let nb = node 'B' n9 nd in
  let n7 = node '7' n3 nb in
  let nh = node 'H' ng ni in
  let nl = node 'L' nk nm in
  let nj = node 'J' nh nl in
  let np = node 'P' no nq in
  let nt = node 'T' ns nu in
  let nj = node 'J' nh nl in
  let nr = node 'R' np nt in
  let nn = node 'N' nj nr in
  let nf = node 'F' n7 nn in
  nf

module rec CharData : Data = struct
  type t = char
  let dataset = fun i -> [|big_tree; big_big_tree|].(i)
  let put = print_char
  include Print(CharData)
end;;


RedBlackData.print @@ RedBlackData.dataset 0;;

CharData.print @@ CharData.dataset 0;;

CharData.print @@ CharData.dataset 1;;


