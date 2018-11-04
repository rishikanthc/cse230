(* Q1. a *)
let length l = 
        let func acc lval = acc + 1 in
        List.fold_left func 0 l;;

(* Q1 b *)
let remove l x =
        let func acc lval =
                if lval = x then acc
                else acc@[lval]
        in List.fold_left func [] l
;;

(* q2 a *)
let rec ith l i d =
        match l with
        | [] -> d
        | h::t -> if i = 0 then h
        else ith t (i-1) d
;;

(* q2 b *)
let rec update l i n =
        match l with
        | [] -> l
        | h::t ->
                if i = 0 then n::t
                else h::(update t (i-1) n)
;;

(* q2 c *)
let rec update2 l i n d =
        match l with
        | [] -> update2 [d] i n d
        | h::t ->
                if i = 0 then n::t
                else h::(update2 t (i-1) n d)
;;

let categorize f l =
        let base = [] in
        let fold_fn acc elmt = 
                let idx = f elmt in
                let bin = ith acc idx [] in
                update2 acc idx (update2 bin (List.length bin) elmt 0) []
        in List.fold_left fold_fn base l
;;

(* q3 *)
let f i= if i<0 then 0
else (if i < 10 then 1
                     else (if i < 20 then 2 else 3));;


