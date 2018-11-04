(*Q1 *)
let rec split l = 
        let pt = (List.length l) / 2 in
        let base = (0, [], []) in
        let fold_fn (i,l1,l2) element =
                if i < pt then (i+1, l1@[element], l2)
                else (i+1, l1, l2@[element])
        in let (_, l1, l2) = List.fold_left fold_fn base l in
        (l1, l2)
;;

(* Q1 b *)
let rec merge l1 l2 =
        match (l1,l2) with
        | ([], l) -> l
        | (l, []) -> l
        | ((h1::t1), (h2::t2)) ->
                        if h2 <= h1 then h2::(merge (h1::t1) t2)
                        else h1::(merge t1 (h2::t2))
;;

(* q1 c *)
let rec merge_sort l = 
        let (left,right) = split l in
        match (left, right) with
        | ([], []) -> []
        | ([], l) -> l
        | (l, []) -> l
        | _ ->
        let sleft = merge_sort left in
        let sright = merge_sort right in
        merge sleft sright
;;

let explode s =
  let rec _exp i =
    if i >= String.length s then [] else (s.[i])::(_exp (i+1)) in
  _exp 0
;;

let implode l =
  let res = String.create (List.length l) in
  let rec imp i = function
  | [] -> res
  | c :: l -> res.[i] <- c; imp (i + 1) l in
  imp 0 l;;

(* q2 *)
let replace l =
        let func y = if y = '-' then ' ' else y in
        implode (List.map func (explode l))
;;

(* q3i a *)
let app l x = List.map (fun f -> f x) l;;
let incr x = x+1;;
let decr x = x-1;;
