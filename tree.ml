type color = Red | Black
type 'a element = VirtualLeft | VirtualRight | VirtualNode | VisibleNode of 'a * bool * bool | VisibleLeft | VisibleRight

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

let rec leftmost_padding_of_depth = function
  | 0 -> 0
  | 1 -> 0
  | 2 -> 2
  | x -> 1 + 2 * leftmost_padding_of_depth (x - 1)

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

let rec is_visual_leaf = function
  | Leaf -> true
  | Node ((_, Some _), _, _) -> false
  | Node ((_, None), _, _) -> true

let rec generate_first_line (lst : ((int * 'a option) tree list)) = match lst with
  | [] -> []
  | (Node ((p, Some c), l, r) as x::t) -> (p, VisibleNode (c, is_visual_leaf l, is_visual_leaf r)) :: generate_first_line t
  | (Node ((p, None), _, _) as x::t) -> (p, VirtualNode) :: generate_first_line t
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
    | VisibleNode (_, left_is_leaf, right_is_leaf) -> (n - offset, if left_is_leaf then VirtualLeft else VisibleLeft) :: (1, if right_is_leaf then VirtualRight else VisibleRight) :: generate_next_line t false
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
    let count_down = max 0 (leftmost_padding_of_depth depth - leftmost_padding_of_depth (depth - 1) - 1) in
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
    | ((n, VisibleNode (c, _, _))::t) -> print_string (String.make n ' '); Tree.print_node c; print_line t
    | ((n, VisibleLeft)::t) -> print_string (String.make n ' '); print_char '/'; print_line t
    | ((n, VisibleRight)::t) -> print_string (String.make n ' '); print_char '\\'; print_line t
    | ((n, _)::t) -> print_string (String.make n ' '); print_char ' '; print_line t
  ;;
  let rec print_helper = function
    | [] -> ()
    | (h::t) -> print_line h; print_helper t;;
  let print x = print_helper @@ get_lines x
end

module RedBlackTree = Printer (struct
  type t = color * char
  let red_text = "\027[31m"
  let reset_color = "\027[0m" 
  let print_node = function
    | (Red, c) -> Printf.printf "%s%c%s" red_text c reset_color
    | (_, c) -> print_char(c)
end);;

module CharTree = Printer (struct
  type t = char
  let print_node = print_char
end);;

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
  let nr = node 'R' np nt in
  let nn = node 'N' nj nr in
  let nf = node 'F' n7 nn in
  nf
;;

CharTree.print big_tree;;
CharTree.print big_big_tree;;
RedBlackTree.print root;;

module Char = struct
  let data = Node ('G', Leaf, Leaf)
  let rec insert c = function
    | Leaf -> Node (c, Leaf, Leaf)
    | Node (v, l, r) as t -> if c == v then t else if c > v then Node (v, l, insert c r) else Node (v, insert c l, r)
  let print = CharTree.print
end;;

Char.(print @@ insert 'A' data);;
Char.(print @@ insert 'H' @@ insert 'A' data);;
Char.(print @@ insert 'C' @@ insert 'H' @@ insert 'A' data);;


module RedBlackDemoPrint = Printer (struct
  type t = color * char * int
  let red_text = "\027[31m"
  let reset_color = "\027[0m" 
  let print_node = function
    | (Red, c, _) -> Printf.printf "%s%c%s" red_text c reset_color
    | (_, c, _) -> print_char(c)
end);;

module RedBlackDemoPrintOrder = Printer (struct
  type t = color * char * int
  let print_node = function
    | (_, _, o) -> print_int o
end);;

module Ordered (Value: sig
  type t
  val equals : t -> t -> bool
  val less_than : t -> t -> bool
end) = struct
  let rec insert v = function
    | Leaf -> Node (v, Leaf, Leaf)
    | Node (nv, lt, rt) as n ->
      if Value.equals v nv then n
      else
        if Value.less_than v nv
        then Node (nv, insert v lt, rt)
        else Node (nv, lt, insert v rt)
  ;;
end;;

module LabledIntTree = Ordered (struct
  type t = color * char * int
  let equals x = function (_, _, yv) -> match x with (_, _, xv) -> xv == yv
  let less_than x = function (_, _, yv) -> match x with (_, _, xv) -> xv < yv
end);;

module LabelTree = Ordered (struct
  type t = color * char
  let equals x = function (_, yv) -> match x with (_, xv) -> xv == yv
  let less_than x = function (_, yv) -> match x with (_, xv) -> xv < yv
end);;

module RedBlackDemo = struct
  let demo1 =
    let a = Node ((Black, 'a', 0), Leaf, Leaf) in
    let b = Node ((Black, 'b', 2), Leaf, Leaf) in
    let x = Node ((Red, 'x', 1), a, b) in
    let c = Node ((Black, 'c', 4), Leaf, Leaf) in
    let y = Node ((Red, 'y', 3), x, c) in
    let d = Node ((Black, 'd', 6), Leaf, Leaf) in
    let z = Node ((Black, 'z', 5), y, d) in
    z
  
  let demo2 =
    let b = Node ((Black, 'b', 2), Leaf, Leaf) in
    let c = Node ((Black, 'c', 4), Leaf, Leaf) in
    let y = Node ((Red, 'y', 3), b, c) in
    let a = Node ((Black, 'a', 0), Leaf, Leaf) in
    let x = Node ((Red, 'x', 1), a, y) in
    let d = Node ((Black, 'd', 6), Leaf, Leaf) in
    let z = Node ((Black, 'z', 5), x, d) in
    z
  
  let demo3 =
    let a = Node ((Black, 'a', 0), Leaf, Leaf) in
    let b = Node ((Black, 'b', 2), Leaf, Leaf) in
    let c = Node ((Black, 'c', 4), Leaf, Leaf) in
    let d = Node ((Black, 'd', 6), Leaf, Leaf) in
    let z = Node ((Red, 'z', 5), c, d) in
    let y = Node ((Red, 'y', 3), b, z) in
    let x = Node ((Black, 'x', 1), a, y) in
    x
  
  let demo4 =
    let a = Node ((Black, 'a', 0), Leaf, Leaf) in
    let b = Node ((Black, 'b', 2), Leaf, Leaf) in
    let c = Node ((Black, 'c', 4), Leaf, Leaf) in
    let d = Node ((Black, 'd', 6), Leaf, Leaf) in
    let y = Node ((Red, 'y', 3), b, c) in
    let z = Node ((Red, 'z', 5), y, d) in
    let x = Node ((Black, 'x', 1), a, z) in
    x

  let greater_than x node = match node with
    | Leaf -> false
    | Node ((_, v), _, _) -> x > v

  let less_than x node = match node with
    | Leaf -> false
    | Node ((_, v), _, _) -> x < v

  let is_red = function
    | Node ((Red, _), _, _) -> true
    | _ -> false
  
  let left_is_red = function
    | Node (_, Node ((Red, x), a, b), _) -> Option.some (x, a, b)
    | _ -> Option.none
  
  let rec fix = function
    | Leaf -> Leaf
    | Node ((Black, z, zo), Node ((Red, y, yo), Node ((Red, x, xo), a, b), c), d) ->
      Node ((Red, y, yo), Node ((Black, x, xo), a, b), Node ((Black, z, zo), c, d))
    | Node ((Black, z, zo), Node ((Red, x, xo), a, Node ((Red, y, yo), b, c)), d) ->
      Node ((Red, y, yo), Node ((Black, x, xo), a, b), Node ((Black, z, zo), c, d))
    | Node ((Black, x, xo), a, Node ((Red, y, yo), b, Node ((Red, z, zo), c, d))) ->
      Node ((Red, y, yo), Node ((Black, x, xo), a, b), Node ((Black, z, zo), c ,d))
    | Node ((Black, x, xo), a, Node ((Red, z, zo), Node ((Red, y, yo), b, c), d)) ->
      Node ((Red, y, yo), Node ((Black, x, xo), a, b), Node ((Black, z, zo), c, d))


end;;

print_endline "*********** demo 1 ***********";;

RedBlackDemoPrintOrder.print RedBlackDemo.demo1;;

RedBlackDemoPrint.print RedBlackDemo.demo1;;

RedBlackDemoPrintOrder.print @@ RedBlackDemo.fix RedBlackDemo.demo1;;

RedBlackDemoPrint.print @@ RedBlackDemo.fix RedBlackDemo.demo1;;

print_endline "*********** demo 2 ***********";;

RedBlackDemoPrintOrder.print RedBlackDemo.demo2;;

RedBlackDemoPrint.print RedBlackDemo.demo2;;

RedBlackDemoPrintOrder.print @@ RedBlackDemo.fix RedBlackDemo.demo2;;

RedBlackDemoPrint.print @@ RedBlackDemo.fix RedBlackDemo.demo2;;

print_endline "*********** demo 3 ***********";;

RedBlackDemoPrintOrder.print RedBlackDemo.demo3;;

RedBlackDemoPrint.print RedBlackDemo.demo3;;

RedBlackDemoPrintOrder.print @@ RedBlackDemo.fix RedBlackDemo.demo3;;

RedBlackDemoPrint.print @@ RedBlackDemo.fix RedBlackDemo.demo3;;

print_endline "*********** demo 4 ***********";;

RedBlackDemoPrintOrder.print RedBlackDemo.demo4;;

RedBlackDemoPrint.print RedBlackDemo.demo4;;

RedBlackDemoPrintOrder.print @@ RedBlackDemo.fix RedBlackDemo.demo4;;

RedBlackDemoPrint.print @@ RedBlackDemo.fix RedBlackDemo.demo4;;

(* RedBlack.(print @@ insert 'A' data);;
RedBlack.(print @@ insert 'C' @@ insert 'A' data);;
RedBlack.(print @@ insert 'H' @@ insert 'C' @@ insert 'A' data);;
RedBlack.(print @@ insert 'C' data);;
RedBlack.(print @@ insert 'H' @@ insert 'C' data);;
RedBlack.(print @@ insert 'H' data);; *)
