(* CSE 130: Programming Assignment 2
 * misc.ml
 *)

(* ***** DOCUMENT ALL FUNCTIONS YOU WRITE OR COMPLETE ***** *)

(* assoc: int * string * (string * int) list -> int
 * takes a list of key-value pairs, a default value and a key to search for. 
 * The function recursively searches for the key in the list of key-val pairs.
 * if the key is found the corresponding value is returned
 * else the default value passed is returned.
 * NOTE: The function is tail recursive
 *)
let rec assoc (d,k,l) = 
        let rec helper x = match x with
        | [] -> d
        | ((a,b)::t) -> if a = k then b else helper t
        in helper l;;

(* fill in the code wherever it says : failwith "to be written" *)
(* removeDuplicates: int list -> int list
 * the function takes a list and returnes the list with successive duplicate occurences removed
 * The function recursively splits the head and checks if it's present in the accumulator(seen)
 * If it's not already present it means it's the first occurence and this element is appended to the accumulator
 * The function is then called recursively on the tail (remaining elements).
 * When no more elemnts are returned, the accumulator is reversed and returned as the final value.
 * NOTE: The function is tail recursive.
 *)
let removeDuplicates l = 
  let rec helper (seen,rest) = 
      match rest with 
        [] -> seen
      | h::t -> 
        let seen' = if (List.mem h seen) then seen else h::seen in
        let rest' = t in 
	  helper (seen',rest') 
  in
      List.rev (helper ([],l))


(* Small hint: see how ffor is implemented below *)
(* wwhile: (int -> int * bool) * int -> int
 * The function takes as input a function and a value and recursively applies the function on the value,
 * each time taking the output of the function as the input for the next call, until the function returns 
 * false as in the output pair generated.
 * NOTE: the function is tail recursive.
 *)
let rec wwhile (f,b) = 
        let rec helper (b',c') = match c' with
        | false -> b'
        | true -> helper((f b'))
        in helper ((f b));;

(* fill in the code wherever it says : failwith "to be written" *)
(* modifier: (int -> int) * int -> int * bool
 * This is a helper function for the fixpoint function.
 * It simply takes the function passed as input and applies it on the int x.
 * if the output of the function is same as that of x then x, false is returned
 * else the output of the function, true is returned
 *)
let modifier g x = 
        if (g x) = x then (x, false)
        else (g x, true);;

(* fixpoint: (int -> int) * int -> int
 * the fixpoint function takes as input a function and an input.
 * It then recursively applies the function on the input, each time taking the output as successive input.
 * It returns when the output produced is the same as the input.
 * The function makes use of the wwhile function and the modifier helper function.*)
let fixpoint (f,b) = wwhile ((modifier f),b)


(* ffor: int * int * (int -> unit) -> unit
   Applies the function f to all the integers between low and high
   inclusive; the results get thrown away.
 *)

let rec ffor (low,high,f) = 
  if low>high 
  then () 
  else let _ = f low in ffor (low+1,high,f)
      
(************** Add Testing Code Here ***************)
