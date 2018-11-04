(* Q1 a *)
type expr = 
        | Const of int
        | Var of string
        | Op of string * expr * expr;;

let rec rename_var e n1 n2 = match e with
        Var s -> if s = n1 then (Var n2) else (Var s)
        | Const i -> (Const i)
        | Op (a, b, c) -> Op (a, (rename_var b n1 n2), (rename_var c n1 n2))
;;

(* Q1 b *)
let to_str e =
        let rec str_helper e top_level =
                match e with
                | Var s -> s
                | Const i -> string_of_int i
                | Op (a, b, c) -> let convtd = (str_helper b false) ^ a ^ (str_helper c false) in
                if top_level then convtd
                else "(" ^ convtd ^ ")"
        in str_helper e true;;

(* Q2 *)
let average_if f l = 
        let folding_fn (count, sum) lval = 
                if f lval then (count + 1, sum + lval)
                else (count, sum) in
        let base = (0, 0) in
        let (n, s) = List.fold_left folding_fn base l in
        if n > 0 then s/n
        else 0
;;

(* Q3 a *)
let length_2 l =
        List.fold_left (+) 0 (List.map List.length l);;

(* Q3 b *)
let length_3 l =
        List.fold_left (+) 0 (List.map length_2 l);;

(* Q4 
 * a [2;4;6;8]
 * b [11;9;7;5]
 * c [3;9;18]
 * d 0 
 * e yyyzzzabc
 * f [24;18;12]*)
