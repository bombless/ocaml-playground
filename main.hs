array =
    [
        ["露娜", "周瑜", "鲁班大师"],
        ["李白", "百里守约", "元歌"],
        ["云樱", "耀", "雅典娜", "司马懿", "裴擒虎", "暃", "橘右京"],
        ["阿柯", "蒙犽", "周瑜"],
        ["孙悟空", "朵莉亚"],
        ["周瑜", "朵莉亚", "海诺", "明世隐"],
        ["周瑜", "朵莉亚", "海诺", "明世隐"],
        ["暃", "蒙犽", "白起", "哪吒"],
        ["盘古", "白起", "孙悟空"],
        ["伽罗", "李白", "女娲"],
        ["耀", "干将莫邪", "伽罗", "花木兰", "太乙真人"],
        ["百里守约", "干将莫邪", "扁鹊"],
        ["露娜"]
    ]

exists x [] = False
exists x (h:t) = x == h || (exists x t)

select x [] = error "no"
select x (h:t) = if x == 0 then h else select (x - 1) t

append [] b = b
append (h:t) b = h : (append t b)

previous 0 lines = []
previous x (h:t) = h : previous (x - 1) t
previous x [] = []


afterwards x [] = []
afterwards 0 x = x
afterwards 1 (_:t) = t
afterwards x (_:t) = afterwards (x - 1) t

next_choice_helper curr line =
    if line >= 12 then Nothing
    else
        let row = select line array in
        let n = select line curr + 1 in
        if n >= length row
        then
            let reset = append (previous line curr) (0 : afterwards (line + 1) curr) in
            next_choice_helper reset (line + 1)
        else
            Just $ append (previous line curr) (n : afterwards (line + 1) curr)

next_choice curr = next_choice_helper curr 0

show_helper [] left = ""
show_helper (h:t) [] = error "wild pointer"
show_helper (h:t) (x:left) = select h x ++ "; " ++ show_helper t left

zeros 0 = []
zeros x = 0 : zeros (x - 1)

initial = zeros 12

show_item pointers = show_helper pointers array

show_all_helper choices =
    case next_choice choices of
        Just next -> show_item choices ++ "\n" ++ show_all_helper next
        Nothing -> ""

show_all = show_all_helper initial


heroes_of_array1 acc [] = acc
heroes_of_array1 acc (h:t) = if exists h acc then heroes_of_array1 acc t else heroes_of_array1 (h:acc) t

heroes_of_array2 acc [] = acc
heroes_of_array2 acc (h:t) = heroes_of_array2 (heroes_of_array1 acc h) t

all_heroes = heroes_of_array2 [] array

apply :: [Int] -> [[String]] -> [String]
apply [] left = []
apply (h:t) left = select h (select 0 left) : apply t (afterwards 1 left)

exists_in_all_choices :: String -> [[String]] -> Bool
exists_in_all_choices x [y] = exists x y
exists_in_all_choices x (h:t) = exists x h && exists_in_all_choices x t

get_choosing_results :: [[Int]] -> [[String]]
get_choosing_results xs = map (`apply` array) xs

filter_all_exists :: [String] -> [[Int]] -> [String]
filter_all_exists [] choices = []
filter_all_exists (h:t) choices =
    if exists_in_all_choices h $ get_choosing_results choices
    then h : filter_all_exists t choices
    else filter_all_exists t choices

all_choices :: [[Int]] -> [Int] -> [[Int]]
all_choices acc curr =
    case next_choice curr of
        Just x -> all_choices (x:acc) x
        Nothing -> acc

result = filter_all_exists all_heroes (all_choices [] initial)

main :: IO ()
main = mapM_ putStrLn result
