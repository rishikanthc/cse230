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

(*
 * sqsum: int list -> int 
 * The function takes a list of integers and returns the sum of squares of the list. 
 * the function uses list.fold_left to apply the square sum function on the list element wise
 * and accumulate the results in a variable and returns it.
 *)
let sqsum xs = 
  let f a x = a + (x*x) in
  let base = 0 in
    List.fold_left f base xs
(*
 * pipe: ('a -> 'a) list -> ('a -> 'a) 
 * the function take s alist of functions and returns a composite function of all of them.
 * The function works by returning a base function which returns the integer argument that it takes as input.
 * Then list.fold is used to form the composite function.
 *)
let pipe fs = 
  let f a x = fun a -> x(x(a)) in
  let base = fun b -> b in
    List.fold_left f base fs

(* sepConcat: string -> string list -> -> string 
 * The function takes as input a separator and an array.
 * The function produces a string which appends the separator to every element
 *)
let rec sepConcat sep sl = match sl with 
  | [] -> ""
  | h :: t -> 
      let f a x = if x="" then a else a^sep^x in
      let base = h in
      let l = t in
        List.fold_left f base l

(* stringOFList: ('a -> string) -> 'a list -> string
 * The function takes as input a list of strings and produces
 * an output string which appends a ; spearator to every string and puts them in square brackets
 *)
let stringOfList f l = "[" ^ (sepConcat "; " (List.map f l)) ^ "]";;

(*****************************************************************)
(******************* 2. Big Numbers ******************************)
(*****************************************************************)

(* clone: 'a -> int -> 'a list
 * The function takes as input an integer and a value
 * and produces an output array which contains a list of values repated
 * number of times as specified by the integer.
 *)
let rec clone x n = 
        let rec helper acc y = if y < 0 then [] else match y with
                | 0 -> acc
                | _ -> helper (x::acc) (y-1)
        in helper [] n;;

(* padZero: int list -> int list -> int list * int list
 * The function takes as input 2 lists and pads zeros in front of the shorter
 * list, to make the lengths of the 2 arrays equal.
 *)
let rec padZero l1 l2 = 
        let len1 = List.length l1 in
        let len2 = List.length l2 in
        if len1 > len2 then l1,(List.append (clone 0 (len1 - len2)) l2)
        else (List.append (clone 0 (len2 - len1)) l1),l2

(* removeZero: int list -> int list
 * The function takes as argument a list, and removes any leading zeros
 *)
let rec removeZero l = match l with
| [] -> []
| (h::t) -> if h = 0 then removeZero t else (h::t)

(* bigAdd: int list -> int list -> int list
 * The function takes as input 2 lists or big integers and produces the sum.
 * The base case is a tuple of carry and a list.
 * The function f keeps adding digit by digit and stores the carry and uses it in subsequent additions.
 *)
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

(* mulByDigit: int -> int list -> int list
 * The function does multiplication, of a big int with a single digit.
 * The function does this by adding the big int repeatedly n times, where n is the integer input
 * it takes as argument.
 *)
let rec mulByDigit i l =
        let rec helper n acc = match n with
        | 0 -> acc
        | _ -> helper (n-1) (bigAdd acc l)
        in helper i [];;

(* addZ: int -> int list
 * The function takes as input an integer n and outputs
 * an array of zeros of length n
 *)
let rec addZ n = match n with
|0 -> []
|_ -> 0::addZ (n-1);;

(* bigMul: int list -> int list -> int list 
 * The function takes as input 2 big ints, and multiplies the 2 of them.
 * The function does this by using mulByDigit. It takes 2 lists, l1, l2.
 * The function performs multiplication in the traditional way.
 * It splits the second list digit by digit, and uses mulByDigit to produce the multiplication answer,
 * then sums the multiplication values of all digits, shifting them appropriately using addZ.
 *)
let bigMul l1 l2 = 
  let f a x = let (var, acc) = a in
  let mval = (mulByDigit x l1)@(addZ var) in
  ((var + 1), bigAdd acc mval) in
  let base = (0, []) in
  let args = List.rev l2 in
  let (_, res) = List.fold_left f base args in
    res
