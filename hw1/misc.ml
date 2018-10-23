(* CSE 130: Programming Assignment 1
 * misc.ml
 *)

(* sumList : int list -> int
   ***** PUT DOCUMENTATION COMMENTS HERE *****
   The functions implements a simple recursive logic to calculate the sum of a list
   of integers.
   NOTE: The function assumes that the input is only integers
   The recursion base case is an empty list which returns 0
   Otherwise, it recursively separates the head of the list and sums it up with the head
   of the next recursive call till it goes to the base case.
*)

let rec sumList l = match l with
    [] -> 0
    | (h::t) -> h + sumList(t);;


(* digitsOfInt : int -> int list
   ***** PUT DOCUMENTATION COMMENTS HERE *****
   The function takes as argument a positive number and returns the digits as a list.

   eg. (digitsOfInt 1234) is [1,2,3,4]
       (digitsOfInt 235) is [2,3,5]
 *)

let rec digitsOfInt n =
    let rec digitsHelper n x =
        if n < 10 then n::x
        else digitsHelper (n/10) ((n mod 10)::x)
    in digitsHelper n [];;

(* digits : int -> int list
 * (digits n) is the list of digits of n in the order in which they appear
 * in n
 * e.g. (digits 31243) is [3,1,2,4,3]
 *      (digits (-23422) is [2,3,4,2,2]
 *)

let digits n = digitsOfInt (abs n)


(* From http://mathworld.wolfram.com/AdditivePersistence.html
 * Consider the process of taking a number, adding its digits,
 * then adding the digits of the number derived from it, etc.,
 * until the remaining number has only one digit.
 * The number of additions required to obtain a single digit from a number n
 * is called the additive persistence of n, and the digit obtained is called
 * the digital root of n.
 * For example, the sequence obtained from the starting number 9876 is (9876, 30, 3), so
 * 9876 has an additive persistence of 2 and a digital root of 3.
 *)


(* ***** PROVIDE COMMENT BLOCKS FOR THE FOLLOWING FUNCTIONS ***** *)
(* additivePersistence : int list -> int
 * The function returns the additivePersistence of the number passed
 *
 * additivePersistence  makes use of sumList and digitsOfInt to count number of additions
 *  eg. additivePersistence 9876;; is 2
 * :*)

let rec additivePersistence n = if n <= 0 then 0
    else let t = sumList(digitsOfInt(n)) in
    if t < 10 then 1
    else 1 + additivePersistence(t);;

(* digitalRoot : int -> int
 * The function takes as input a positive number and returns the digitalRoot
 * makes use of the sumList and digitsOfInt functions to first split
 * the number into it's digits and then sums it up.
 * e.g. digitalRoot 9876 is 3
 *  *)
let rec digitalRoot n = if n < 10 then n
    else digitalRoot(sumList(digitsOfInt(n)));;

(* listReverse : a' list -> a' list
 * The function  takes as input a list and returns the reverse of that list
 * It uses recursive helper to remove the head and cons it to the list form the previous
 * recursive call
 * e.g listReverse [1;2;3;4] is [4;3;2;1]
 * *)
let rec listReverse l =
    let rec revHelp l x = match l with
        [] -> x
        | (h::t) -> revHelp t (h::x)
    in revHelp l [];;

(* explode : string -> char list
 * (explode s) is the list of characters in the string s in the order in
 *   which they appear
 * e.g.  (explode "Hello") is ['H';'e';'l';'l';'o']
 *)
let explode s =
  let rec _exp i =
    if i >= String.length s then [] else (s.[i])::(_exp (i+1)) in
  _exp 0

(* palindrome : int -> int
 * The function simply compares the reverse of the list (using the listReverse function)
 * and returns true if they are the same and else returns false
 *
 * e.g. palindrome "Malayalam" is true
 * *)
let palindrome w =
    let strL = explode w in
        if strL = listReverse strL then true
        else false;;

(************** Add Testing Code Here ***************)
