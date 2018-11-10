(* CSE 130 10/31 *)

(* WI 12 Midterm, 3a *)
let app l x =
   List.map (fun y -> y x) l;;

(* FA 13 Midterm, 2a *)
let fold_2 f b l =
   let base = (0,b) in
   let fold_fn (idx,acc) elmt = (idx+1, f acc elmt idx) in
   let (_,res) = List.fold_left fold_fn base l in res;;

(* FA 13 Midterm, 2b *)
let rec ith l i d = 
   let base = d
   let fold_fn acc elmt idx =
    match idx=i with
    | true -> elmt
    | false -> acc in
   fold_2 fold_fn base l;; 

(* FA 13 Midterm, 3a*)
type ’a fun_tree =
| Leaf of (’a -> ’a)
| Node of (’a fun_tree) * (’a fun_tree);;

let rec apply_all t x =
   match t with
   | Node (l,r) -> apply_all r (apply_all l x)
   | Leaf f -> f x;;






















(* CSE 230 10/31 *)

(* Spring 13 Midterm, 2 *)

let rec ith l i d =
   match l with
   | [] -> d
   | h::t -> if i = 0 then h else ith t (i-1) d;;

let rec update l i n =
  match l with 
  | [] -> []
  | h::t -> if i = 0 then n::t else h::(update t (i-1) n);;


let rec update_2 l i n d =
  match l with 
  | [] -> update_2 [d] i n d
  | h::t -> if i = 0 then n::t else h::(update_2 t (i-1) n d);;


let categorize f l =
   let base = [] in
   let fold_fn acc elmt =  
        update_2 acc (f elmt) ((ith acc (f elmt) [])@[elmt]) [] in
   in List.fold_left fold_fn base l;;























(* CSE 130 11/2*)

(* Fa 14 Midterm Q1 *)
let rec rename_var e n1 n2 =
match e with
| Const i -> e
| Var s -> Var (if s = n1 then n2 else s) 
| Op (s,e1,e2) -> Op(s, rename_var e1 n1 n2, rename_var e2 n1 n2)

let to_str e =
   let rec str_helper e top_level =
   match e with
   | Const i -> string_of_int i
   | Var s -> s
   | Op (s,e1,e2) ->
         let op_str = (str_helper e1 false)^s^(str_helper e2 false) in
         if top_level then op_str else "(" ^ op_str ^ ")" 
in str_helper e true;;

(* Write map and fold *)
let rec map f l =
   match l with
   | [] -> []
   | h::t -> (f h)::(map f t);;

let rec fold_left f acc l =
   match l with
   | [] -> acc
   | h::t -> fold_left f (f acc h) t;; 

let f1 = List.map (fun x->2*x);;
f1 [1;2;3;4];;

let f2 = List.fold_left (fun x y -> (y+2)::x) [];;
f2 [3;5;7;9];;

let f3 = List.fold_left (fun x y -> x@[3*y]) [];;
f3 [1;3;6];;
[3;9;18]

let f = List.fold_left (fun x y -> y x);;
f 1 [(+) 1; (-) 2];;
0
f "abc" [(^) "zzz"; (^) "yyy"];;
yyyzzzabc

f [1;2;3] [f1;f2;f3];;
[24;18;12]
























(* CSE 230 11/2 *)


(* Winter 13 Final Q1 *)
let sum_matrix l =
  let fold_fn acc elmt = List.fold_left (+) acc elmt in
  List.fold_left fold_fn 0 l;;

let map f l =
 match l with
 | [] -> []
 | h::t -> (f h)::map f t;;

let fold_left f acc l =
  match l with 
  | [] -> acc
  | h::t -> fold_left f (f acc h) t;;

(*WI 11 final Q1 *)
type ’a dict = Empty | Node of string * ’a * ’a dict * ’a dict

let rec find d k =
   match d with
   | Empty -> raise Not_found
   | Node (k’, v’, l, r) ->
       if k = k’ then v' else
       if k < k’ then find l k else
         (* k > k’ *) find r k;;

let rec add d k v =
match d with
   | Empty -> Node (k, v, Empty, Empty)
   | Node (k’, v’, l, r) ->
       if k = k’ then Node(k,v,l,r) else
       if k < k’ then Node(k',v',add l k v,r) else
       (* k > k’ *)   Node(k',v',l,add r k v)

let rec fold f b d =
match d with
| Empty -> b 
| Node (k, v, l, r) ->
    fold f (f k v (fold f b l)) r;;

(* Final Fall 05 Q5 *)
type boolexp =
  | Var of int
  | Not of boolexp
  | Or of boolexp*boolexp
  | And of boolexp*boolexp;;

let ith l i = 
  match l with
  | [] -> false
  | h::t -> if i = 0 then h else ith t (i-1)

let rec eval l e =
match e with
| Var i -> ith l i
| Not e -> not (eval l e) 
| And (e1,e2) -> (eval l e1) && (eval l e2)
| Or (e1,e2) -> (eval l e1) || (eval l e2)

let rec inputs n = 
   if n = 0 then [[]]
   else let l = inputs (n-1) in
          (List.map (fun x -> true::x) l)@
          (List.map (fun x -> false::x) l);;
