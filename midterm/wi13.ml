(* Q1 a *)
type 'a maybe =
| None
| Some of 'a

let first f l =
        let base = None in
        let fold_fn acc elmt = 
                match acc with
                | None -> if f elmt then Some elmt else None
                | Some x -> Some x
        in List.fold_left fold_fn base l
;;

(* Q2 a *)
let rec zip l1 l2 =
        match (l1, l2) with
        | ([], l) -> []
        | (l, []) -> []
        | ((h1::t1), (h2::t2)) -> (h1,h2)::(zip t1 t2)
;;

(* q2 b *)
let map2 f l1 l2 = 
        let func (a,b) = f a b in
        List.map func (zip l1 l2);;

(* Q2 c *)
let map3 f l1 l2 l3 = 
        let func (a,(b,c)) = f a b c in
        List.map func (zip l1 (zip l2 l3))
;;

(* Q3 *)
let rec unzip l =
        match l with
        | [] -> ([],[])
        | (a,b)::t -> let (t1, t2) = unzip t in
                        (a::t1, b::t2)
;;
