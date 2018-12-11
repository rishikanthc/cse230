type 'a tree=
| Empty
| Node of 'a * 'a tree list;;

exception Mismatch of string

let rec zip l1 l2 =
match (l1,l2) with
| ([],[]) -> []
| (h1::t1, h2::t2) -> (h1,h2)::(zip t1 t2)
| _ -> raise (Mismatch "blah");;


let rec tree_zip t1 t2 = 
        match (t1, t2) with
        | (Empty, Empty) -> Empty
        | (Node (a,l1), Node(b, l2)) -> let z = zip l1 l2 in
                                        let func (n1, n2) = tree_zip n1 n2 in
                                        Node ((a,b), List.map func z)
        | _ -> raise (Mismatch "blah")
;;
