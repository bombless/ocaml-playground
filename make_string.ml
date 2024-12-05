open Random
open Unix

(* compile with `ocamlopt unix.cmxa hash.ml tree.ml make_string.ml -o s` *)

(* 生成一个随机长度为 len 的字符串 *)
let random_string len =
  let chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789" in
  let chars_len = String.length chars in
  String.init len (fun _ -> chars.[Random.int chars_len])

(* 生成 n 条随机字符串，长度在 min_len 到 max_len 之间 *)
let generate_string min_len max_len =
  let len = min_len + Random.int (max_len - min_len + 1) in
  random_string len

let rec insert_strings f count_down acc min_len max_len =
  if count_down > 0 then let acc = f (generate_string min_len max_len) acc in insert_strings f (count_down - 1) acc min_len max_len
  else acc

let rec test_mem f count_down t min_len max_len =
  if count_down > 0
  then let s = generate_string min_len max_len in f s t || test_mem f (count_down - 1) t min_len max_len
  else true

let () =
  Random.self_init (); (* 初始化随机数种子 *)

  let insert_start_time = gettimeofday () in

  let tree = insert_strings Tree.StringTree.insert 1_000_000 Tree.Leaf 1 100 in

  let insert_end_time = gettimeofday () in

  let mem_start_time = gettimeofday () in

  let r = test_mem Tree.StringTree.mem 1_000_000 tree 1 100 in

  let mem_end_time = gettimeofday () in

  Printf.printf "red-black-tree: insert test 1,000,000 strings in %.3f seconds\n" (insert_end_time -. insert_start_time);
  Printf.printf "red-black-tree: mem test 1,000,000 strings in %.3f seconds\n" (mem_end_time -. mem_start_time);


  Random.self_init (); (* 初始化随机数种子 *)

  let insert_start_time = gettimeofday () in

  let tree = insert_strings Hash.insert 1_000_000 (Hash.empty ()) 1 100 in

  let insert_end_time = gettimeofday () in

  let mem_start_time = gettimeofday () in

  let r = test_mem Hash.mem 1_000_000 tree 1 100 in

  let mem_end_time = gettimeofday () in

  Printf.printf "hash: insert test 1,000,000 strings in %.3f seconds\n" (insert_end_time -. insert_start_time);
  Printf.printf "hash: mem test 1,000,000 strings in %.3f seconds\n" (mem_end_time -. mem_start_time);
