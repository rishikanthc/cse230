(* Q1.a *)
let rec insert l i =
        let rec helper (acc, prev) rest =
                match rest with
                | [] -> acc@[i]
                | h::t -> if h > i then
                        (match prev with
                        | None -> i::rest
                        | Some a -> if a <= i then acc@[i]@rest
                                    else helper (acc@[h], Some h) t)
                else helper (acc@[h], Some h) t 
        in helper ([], None) l
;;

(* Q1.b *)
let insertion_sort l = List.fold_left insert [] l;;

(* Q2 *)
type expr =
| Var of string
| Const of int
| Plus of expr * expr
;;

let rec simpl e =
        match e with
        | Plus (a,b) ->
                        let e1' = simpl a in
                        let e2' = simpl b in (
                        match (e1', e2') with
                        | (Const c, Const d) -> Const (c + d)
                        | _ -> Plus (e1', e2')
                        )
        | _ -> e
;;
