
let array = [
    ["露娜"; "周瑜"; "鲁班大师"];
    ["李白"; "百里守约"; "元歌"];
    ["云樱"; "耀"; "雅典娜"; "司马懿"; "裴擒虎"; "暃"; "橘右京"];
    ["阿柯"; "蒙犽"; "周瑜"];
    ["孙悟空"; "朵莉亚"];
    ["周瑜"; "朵莉亚"; "海诺"; "明世隐"];
    ["周瑜"; "朵莉亚"; "海诺"; "明世隐"];
    ["暃"; "蒙犽"; "白起"; "哪吒"];
    ["盘古"; "白起"; "孙悟空"];
    ["伽罗"; "李白"; "女娲"];
    ["耀"; "干将莫邪"; "伽罗"; "花木兰"; "太乙真人"];
    ["百里守约"; "干将莫邪"; "扁鹊"]
]

let rec exists x = function
    | [] -> false
    | h::t -> h == x || (exists x t)



let rec select x = function
    | [] -> failwith "no match"
    | h::t -> if x == 0 then h else select (x - 1) t

let rec append a b = match a with
    | [] -> b
    | h::t -> h :: (append t b)

let rec previous x lines = if x == 0 then [] else (match lines with | h :: t -> h :: previous (x - 1) t | [] -> [])

let rec afterwards x = function
    | [] -> []
    | h :: t -> if x == 0 then h :: t else if x == 1 then t else afterwards (x - 1) t


let rec length = function
    | [] -> 0
    | _::t -> 1 + length t

let rec next_choice_helper curr line = 
    if line >= 12 then failwith "done"
    else
        let row = select line array in
        let n = select line curr + 1 in
        if n >= length row
        then let reset = append (previous line curr) (0 :: (afterwards (line + 1) curr)) in next_choice_helper reset (line + 1)
        else append (previous line curr) (n :: (afterwards (line + 1) curr))

let next_choice curr = next_choice_helper curr 0

let rec show_helper pointers left = match pointers with
    | [] -> ""
    | h :: t -> match left with | [] -> assert false | x :: left -> select h x ^ "; " ^ show_helper t left

let rec zeros x = if x == 0 then [] else 0 :: zeros (x - 1)

let initial = zeros 12

let show pointers = try show_helper pointers array with _ -> ""

let rec show_all_helper choices =
    try 
      let next = next_choice choices in
      show choices ^ "\n" ^ show_all_helper next
    with _ -> ""

(* let show_all = show_all_helper initial *)

let rec heros_of_array1 acc left =
    match left with
    | [] -> acc
    | h :: t -> if exists h acc then heros_of_array1 acc t else heros_of_array1 (h :: acc) t

let rec heros_of_array2 acc left =
    match left with
    | [] -> acc
    | h :: t -> heros_of_array2 (heros_of_array1 acc h) t

type record = {
    hero: string;
    count: int;
}

let rec rec_exists x = function
    | [] -> false
    | {hero = hero}::t -> hero == x || (rec_exists x t)

let rec count_of_hero x = function
    | [] -> 0
    | {hero = hero; count = count}::t -> if hero == x then count else count_of_hero x t

let rec count_and_extract x = function
    | [] -> 0, []
    | {hero = hero; count = count} :: t ->
        if x == hero
        then count, t
        else let c, remain = count_and_extract x t in
            c, {hero = hero; count = count}::remain

let rec apply choices left =
    match choices with
    | [] -> []
    | h :: t -> select h (select 0 left) :: apply t (afterwards 1 left)

let all_heros = heros_of_array2 [] array

let rec count_hero x = function
    | [] -> 0
    | h :: t -> count_hero x t + if x == h then 1 else 0

let rec count_all left heros = match left with
    | [] -> []
    | h :: t -> {hero = h; count = count_hero h heros} :: count_all t heros

let rec above_one = function
    | [] -> []
    | {hero = hero; count = count} :: t -> if count > 0 then {hero = hero; count = count} :: above_one t else above_one t

let min a b = if a > b then b else a

let rec merge_one x lst =
    let {hero = hero; count = count} = x in
    let c, remain = count_and_extract hero lst in
    if c > 0 && count > 0 then [{hero = hero; count = min count c}] else []

let rec merge a = function
    | [] -> a
    | h :: t -> merge (merge_one h a) t

let rec filter_all choices = try
    let heros = apply choices array in
    let tail = above_one (count_all all_heros heros) in
    let next = next_choice choices in
    merge tail (filter_all next)
    with _ -> []

let test_merge_one = merge_one {hero = "百里守约"; count = 1} []

(* let ok = filter_all initial *)

let is_match x heros =
    let {hero = hero} = x in
    exists hero heros

let rec has_match lst heros = match lst with
    | [] -> false
    | h :: t -> if is_match h heros then has_match t heros else false


let rec filter_good choices ok = try
    let heros = apply choices array in
    let next = next_choice choices in
    if has_match ok heros then heros :: filter_good next ok else filter_good next ok
    with _ -> []

let filter_good_result ok = filter_good initial ok

let l = {hero = "露娜"; count = 1}
let test = ["周瑜"; "李白"; "云樱"; "阿柯"; "孙悟空"; "周瑜"; "周瑜"; "暃"; "盘古"; "伽罗"; "耀"; "百里守约"]

let rec next n acc = if n == 0 then acc else next (n - 1) (next_choice acc)