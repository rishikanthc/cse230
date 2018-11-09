exception MLFailure of string

type binop = 
  Plus 
| Minus 
| Mul 
| Div 
| Eq 
| Ne 
| Lt 
| Le 
| And 
| Or          
| Cons

type expr =   
  Const of int 
| True   
| False      
| NilExpr
| Var of string    
| Bin of expr * binop * expr 
| If  of expr * expr * expr
| Let of string * expr * expr 
| App of expr * expr 
| Fun of string * expr    
| Letrec of string * expr * expr
	
type value =  
  Int of int		
| Bool of bool          
| Closure of env * string option * string * expr 
| Nil                    
| Pair of value * value     
| NativeFunc of string

and env = (string * value) list

let binopToString op = 
  match op with
      Plus -> "+" 
    | Minus -> "-" 
    | Mul -> "*" 
    | Div -> "/"
    | Eq -> "="
    | Ne -> "!="
    | Lt -> "<"
    | Le -> "<="
    | And -> "&&"
    | Or -> "||"
    | Cons -> "::"

let rec valueToString v = 
  match v with 
    Int i -> 
      Printf.sprintf "%d" i
  | Bool b -> 
      Printf.sprintf "%b" b
  | Closure (evn,fo,x,e) -> 
      let fs = match fo with None -> "Anon" | Some fs -> fs in
      Printf.sprintf "{%s,%s,%s,%s}" (envToString evn) fs x (exprToString e)
  | Pair (v1,v2) -> 
      Printf.sprintf "(%s::%s)" (valueToString v1) (valueToString v2) 
  | Nil -> 
      "[]"
  | NativeFunc s ->
      Printf.sprintf "Native %s" s

and envToString evn =
  let xs = List.map (fun (x,v) -> Printf.sprintf "%s:%s" x (valueToString v)) evn in
  "["^(String.concat ";" xs)^"]"

and exprToString e =
  match e with
      Const i ->
        Printf.sprintf "%d" i
    | True -> 
        "true" 
    | False -> 
        "false"
    | NilExpr -> 
        "[]"
    | Var x -> 
        x
    | Bin (e1,op,e2) -> 
        Printf.sprintf "%s %s %s" 
        (exprToString e1) (binopToString op) (exprToString e2)
    | If (e1,e2,e3) -> 
        Printf.sprintf "if %s then %s else %s" 
        (exprToString e1) (exprToString e2) (exprToString e3)
    | Let (x,e1,e2) -> 
        Printf.sprintf "let %s = %s in \n %s" 
        x (exprToString e1) (exprToString e2) 
    | App (e1,e2) -> 
        Printf.sprintf "(%s %s)" (exprToString e1) (exprToString e2)
    | Fun (x,e) -> 
        Printf.sprintf "fun %s -> %s" x (exprToString e) 
    | Letrec (x,e1,e2) -> 
        Printf.sprintf "let rec %s = %s in \n %s" 
        x (exprToString e1) (exprToString e2) 

(*********************** Some helpers you might need ***********************)

let rec fold f base args = 
  match args with [] -> base
    | h::t -> fold f (f(base,h)) t

let listAssoc (k,l) = 
  fold (fun (r,(t,v)) -> if r = None && k=t then Some v else r) None l

(*********************** Your code starts here ****************************)

let lookup (x,evn) = 
        match x with
        | "map" -> Closure ([], None, "f", Fun ("list", Letrec ("loop", Fun ("array", If (App (Var "null", Var "array"), NilExpr, Bin (App (Var "f", App (Var "hd", Var "array")), Cons, App (Var "loop", App (Var "tl", Var "array"))))), App (Var "loop", Var "list"))))
        | _ -> let tval = listAssoc (x, evn) in
                match tval with
                | None -> raise (MLFailure ("unbound value: " ^ x))
                | Some a -> a
;;


let convert x = match x with
| Int a -> a
| Bool a -> if a then 1 else 0
;;

let typeCheck (op, x,y) = match (op, x,y) with
| (Plus, Int _, Int _) -> true
| (Minus, Int _, Int _) -> true
| (Mul, Int _, Int _) -> true
| (Div, Int _, Int _) -> true
| (Lt, Int _, Int _) -> true
| (Le, Int _, Int _) -> true
| (Eq, Int _, Int _) -> true
| (Eq, Bool _, Bool _) -> true
| (Ne, Bool _, Bool _) -> true
| (Ne, Int _, Int _) -> true
| (And, Bool _, Bool _) -> true
| (Or, Bool _, Bool _) -> true
| (Cons, _, _) -> true (**)
| _ -> false

let doOp (num1, op, num2) = match op with
        | Plus -> Int ((+) (convert num1) (convert num2))
        | Minus -> Int ((-) (convert num1) (convert num2))
        | Mul -> Int (( * ) (convert num1) (convert num2))
        | Div -> Int ((/) (convert num1) (convert num2))
        | Lt -> Bool ((convert num1) < (convert num2))
        | Le -> Bool ((convert num1) <= (convert num2))
        | Eq -> Bool ((convert num1) = (convert num2))
        | Ne -> Bool ((convert num1) != (convert num2))
        | And -> Bool (((convert num1) * (convert num2)) = 1)
        | Or -> Bool (((convert num1) + (convert num2)) = 1)
        | Cons -> match (num1, num2) with
                        | (_, Nil) -> Pair (num1, Nil)
                        | (_, Pair (a,b)) -> Pair (num1, num2)
        | _ -> raise (MLFailure ("unknown operation"))
;;

let rec eval (evn,e) = match e with
       | Const x1 -> Int x1
       | Var x1 -> 
                       (match x1 with
                       | "hd" -> NativeFunc "hd"
                       | "tl" -> NativeFunc "tl"
                       | "null" -> NativeFunc "null"
                       | _ -> lookup (x1, evn))
       | True -> Bool true
       | False -> Bool false
       | NilExpr -> Nil
       | Let (b, e1, e2) -> eval ((b, eval(evn,e1))::evn ,e2)
       | Letrec (b, e1, e2) -> 
                       let rename x = match x with
                       | Closure (fenv, name, param, body) -> Closure (fenv, Some b, param, body)
                       | _ -> x
                       in
                       eval ((b, rename (eval (evn,e1)))::evn ,e2)
       | Fun (param, body) -> Closure (evn, None, param, body)
       | App (e1, e2) -> 
                       let func = eval (evn, e1) in
                       (match func with
                       | NativeFunc "hd" ->
                                       let list_eval = eval (evn, e2) in
                                       (match list_eval with
                                       | Pair (first, rest) -> first
                                       | Nil -> raise (MLFailure ("list is empty"))
                                       | _ -> raise (MLFailure ("type error: List elements should be a pair")))
                       | NativeFunc "tl" -> 
                                       let list_eval = eval (evn, e2) in
                                       (match list_eval with
                                       | Pair (first, rest) -> rest 
                                       | Nil -> raise (MLFailure ("list is empty"))
                                       | _ -> raise (MLFailure ("type error: List elements should be a pair")))
                       | NativeFunc "null" -> Bool (eval (evn, e2) = Nil)
                       | Closure _ -> 
                               let Closure (fenv, name, arg, body) = func in
                               (match name with
                               | None -> eval ((arg, eval (evn, e2))::fenv, body)
                               | Some f -> eval ((f, func)::(arg, eval (evn, e2))::fenv, body))
                       | _ -> raise (MLFailure ("E1 should evaluate to a function")))
       | If (p, t, f) -> if eval (evn, p) = Bool true then eval (evn, t)
                         else eval (evn, f)
       | Bin (e1, op, e2) -> 
                       let n1 = eval (evn, e1) in
                       let n2 = eval (evn,e2) in
                       if typeCheck (op, n1, n2) then (doOp (n1, op, n2))
                       else raise (MLFailure ("type error"))
       | _ -> raise (MLFailure ("Unknown expression"))
;;

(**********************     Testing Code  ******************************)
