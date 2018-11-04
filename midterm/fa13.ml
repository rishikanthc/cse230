(* Q1. a *)
let count l x = 
        let tester acc lval =
                if lval = x then acc + 1
                else acc
        in List.fold_left tester 0 l;;

(* Q1. b *)
let make_palyndrome l =
        let func acc lval = lval::acc
        in List.fold_left func l l;;

(* Q2 a *)
let fold_2 f b l =
        let base = (b, 0) in
        let func (acc, idx) lval = ((f acc lval idx), idx+1) in
        List.fold_left func base l;;

(* Q2 b *)
let rec ith l i d =
        let func acc lval idx =
                if idx = i then lval
                else acc
        in let (result, _) = fold_2 func d l
        in result
;;

(* Q3 a *)
type 'a fun_tree =
   | Leaf of ('a -> 'a)
   | Node of ('a fun_tree) * ('a fun_tree);;

let rec apply_all t x =
                match t with
                | Leaf f -> (f x)
                | Node (a,b) -> apply_all b (apply_all a x)
;;

let f1 x = x + 1;;
let f2 x = x * 2;;
let f3 x = x + 3;;
let t = Node(Leaf f1, Node(Leaf f2, Leaf f3));;
