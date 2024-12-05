type hash_table = {
  mutable size : int;
  n_buckets : int;
  buckets : string list option array;
}

let empty (_: unit) = {
  size = 0;
  n_buckets = 8;
  buckets = Array.make 8 Option.none
}

let make n = {
  size = 0;
  n_buckets = n;
  buckets = Array.make n Option.none
}

let hash s =
  let ret = ref 0 in
  let len = String.length s in
  String.iter (fun c -> ret := !ret * 31 + Char.code c) s;
  !ret

let rec insert_to_list v = function
  | [] -> [v]
  | (h::_) as lst when h > v -> v :: h :: lst
  | (h::_) as lst when h == v -> lst
  | (h::t) -> h :: insert_to_list v t

let to_list hsh =
  let ret = ref [] in
  Array.iter (fun x -> match x with Option.Some x -> ret := !ret @ x | _ -> ()) hsh.buckets;
  !ret

let get_bucket v hsh = let nth = hash v mod hsh.n_buckets in if nth > 0 then nth else -nth

let mem v hsh =
  let nth = get_bucket v hsh in
  match hsh.buckets.(nth) with
    | Option.Some lst -> List.mem v lst
    | _ -> false

let rec insert v hsh : hash_table =
  let nth = get_bucket v hsh in
  let buckets = hsh.buckets in
  (match buckets.(nth) with
    | Option.Some lst ->      
        let new_list = insert_to_list v lst in
        if List.length new_list > List.length lst then buckets.(nth) <- Option.some @@ new_list; hsh.size <- hsh.size + 1
    | Option.None ->
        buckets.(nth) <- Option.some @@ [v]; hsh.size <- hsh.size + 1);
  if hsh.size >= 2 * hsh.n_buckets then
    let new_tbl = make (2 * hsh.n_buckets) in
    Printf.printf "rehashing from %d to %d\n" hsh.n_buckets (2 * hsh.n_buckets);
    List.iter (fun x -> ignore (insert x new_tbl)) (to_list hsh);
    print_endline "done";
    new_tbl
  else
    hsh;;
