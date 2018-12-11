let rec foldr f xs b =
        let rec helper acc x =
                match x with
                | [] -> acc
                | [h]@[x] -> helper (f x acc) [h]
        in helper b xs
;;

let rec foldra f xs b =
match xs with
[] -> b
| x::xs' -> f x (foldr f xs' b)
;;
