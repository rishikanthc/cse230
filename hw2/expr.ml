(*
 * expr.ml
 * cse130
 * based on code by Chris Stone
 *)

(* Please do not modify the names or types of any of the following
 * type constructors, or predefined functions, unless EXPLICITLY
 * asked to. You will loose points if you do.
 *)


(* REMEMBER TO DOCUMENT ALL FUNCTIONS THAT YOU WRITE OR COMPLETE *)

type expr = 
    VarX
  | VarY
  | Sine     of expr
  | Cosine   of expr
  | Average  of expr * expr
  | Times    of expr * expr
  | Thresh   of expr * expr * expr * expr	
  | Expo     of expr
  | Custom   of expr * expr * expr

(* exprToString expr -> string*
 * The function takes as input an expr, and then proceeds to recursively match it with one of the sub-types of expr. Once it matches, it returns the corresponding equivalent string expression of the operation that is performed. The strings returned are concatenated recursively to produce the entire mathematical representation of the expression as a string.
 * NOTE - The 2 custom operators added are, Sin( exponential)
 * and Cos(pi * exponential (a * b) * c)
 *)
let rec exprToString e = match e with
  | Thresh (a,b,c,d) -> exprToString(a) ^ "<" ^ exprToString(b) ^ "?" ^ exprToString(c) ^ ":" ^ exprToString(d)
  | VarX -> "x"
  | VarY -> "y"
  | Times (a,b) -> exprToString(a) ^ "*" ^ exprToString(b)
  | Sine a -> "sin" ^ "(pi*" ^ exprToString(a) ^ ")"
  | Cosine a -> "cos" ^ "(pi*" ^ exprToString(a) ^ ")"
  | Average (a,b) -> "((" ^ exprToString(a) ^ "+" ^ exprToString(b) ^ ")/2)"
  | Expo (a) -> "sin (e^(" ^ exprToString(a) ^ "))"
  | Custom (a,b,c) -> "cos (e^(" ^ exprToString(a) ^ "*" ^ exprToString(b) ^ ") *" ^ exprToString(c) ^ ")"
;;

(* build functions:
     Use these helper functions to generate elements of the expr
     datatype rather than using the constructors directly.  This
     provides a little more modularity in the design of your program *)

(* buildExpo and buildCustom are added to generate the 2 new operators as specified in the previous function *)
let buildX()                       = VarX
let buildY()                       = VarY
let buildSine(e)                   = Sine(e)
let buildCosine(e)                 = Cosine(e)
let buildAverage(e1,e2)            = Average(e1,e2)
let buildTimes(e1,e2)              = Times(e1,e2)
let buildThresh(a,b,a_less,b_less) = Thresh(a,b,a_less,b_less)
let buildExpo(a)                   = Expo(a)
let buildCustom(a,b,c)             = Custom(a,b,c)


let pi = 4.0 *. atan 1.0

(*eval expr * int * int -> int
 * The function takes as input the expr and proceeds to mathematically evaluate the expression.
 * The function recurses to the innermost operation and then proceeds to evaluate it and then returns the value back to the previous recursive call 
 * *)
let rec eval (e,x,y) = match e with
   | Thresh (a,b,c,d) -> if eval(a,x,y) < eval(b,x,y) then eval(c,x,y) else eval(d,x,y)
   | VarX -> x
   | VarY -> y
   | Times (a,b) -> eval(a,x,y) *. eval(b,x,y)
   | Sine a -> sin (pi *. eval(a,x,y))
   | Cosine a -> cos (pi *. eval(a,x,y))
   | Average (a,b) -> (eval(a,x,y) +. eval(b,x,y) ) /. 2.0
   | Expo(a) -> sin (exp (eval(a,x,y))) 
   | Custom (a,b,c) -> cos (exp (eval(a,x,y) *. eval(b,x,y)) *. eval(c,x,y))
;;

(* (eval_fn e (x,y)) evaluates the expression e at the point (x,y) and then
 * verifies that the result is between -1 and 1.  If it is, the result is returned.  
 * Otherwise, an exception is raised.
 *)
let eval_fn e (x,y) = 
  let rv = eval (e,x,y) in
  assert (-1.0 <= rv && rv <= 1.0);
  rv

let sampleExpr =
      buildCosine(buildSine(buildTimes(buildCosine(buildAverage(buildCosine(
      buildX()),buildTimes(buildCosine (buildCosine (buildAverage
      (buildTimes (buildY(),buildY()),buildCosine (buildX())))),
      buildCosine (buildTimes (buildSine (buildCosine
      (buildY())),buildAverage (buildSine (buildX()), buildTimes
      (buildX(),buildX()))))))),buildY())))

let sampleExpr2 =
  buildThresh(buildX(),buildY(),buildSine(buildX()),buildCosine(buildY()))


(************** Add Testing Code Here ***************)
