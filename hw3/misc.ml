(* CSE 130: Programming Assignment 3
 * misc.ml
 *)

(* For this assignment, you may use the following library functions:

   List.map
   List.fold_left
   List.fold_right
   List.split
   List.combine
   List.length
   List.append
   List.rev

   See http://caml.inria.fr/pub/docs/manual-ocaml/libref/List.html for
   documentation.
*)



(* Do not change the skeleton code! The point of this assignment is to figure
 * out how the functions can be written this way (using fold). You may only
 * replace the   failwith "to be implemented"   part. *)



(*****************************************************************)
(******************* 1. Warm Up   ********************************)
(*****************************************************************)

let sqsum xs = 
  let f a x = a + (x*x) in
  let base = 0 in
    List.fold_left f base xs

let pipe fs = 
  let f a x = fun a -> x(x(a)) in
  let base = fun b -> b in
    List.fold_left f base fs

let rec sepConcat sep sl = match sl with 
  | [] -> ""
  | h :: t -> 
      let f a x = if x="" then a else a^sep^x in
      let base = h in
      let l = t in
        List.fold_left f base l

let stringOfList f l = "[" ^ (sepConcat "; " (List.map f l)) ^ "]";;

(*****************************************************************)
(******************* 2. Big Numbers ******************************)
(*****************************************************************)

let rec clone x n = 
        let rec helper acc y = if y < 0 then [] else match y with
                | 0 -> acc
                | _ -> helper (x::acc) (y-1)
        in helper [] n;;

let rec padZero l1 l2 = 
        let len1 = List.length l1 in
        let len2 = List.length l2 in
        if len1 > len2 then l1,(List.append (clone 0 (len1 - len2)) l2)
        else (List.append (clone 0 (len2 - len1)) l1),l2

let rec removeZero l = match l with
| [] -> []
| (h::t) -> if h = 0 then removeZero t else (h::t)

let bigAdd l1 l2 = 
  let add (l1, l2) = 
          let f a x = let (x1,x2) = x in
          let (_, h::t) = a in 
          let sumv = x1 + x2 + h in
          if sumv > 9 then (1, 1::(sumv mod 10)::t) 
          else (0, 0::sumv::t) in
    let base = (0, [0]) in
    let args = List.rev (List.combine l1 l2) in
    let (_, res) = List.fold_left f base args in
      res
  in 
    removeZero (add (padZero l1 l2))

let rec mulByDigit i l = failwith "to be implemented"

let bigMul l1 l2 = 
  let f a x = failwith "to be implemented" in
  let base = failwith "to be implemented" in
  let args = failwith "to be implemented" in
  let (_, res) = List.fold_left f base args in
    res
