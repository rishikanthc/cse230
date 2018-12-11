exception Foo of string

type 'a dict = Empty | Node of string * 'a * 'a dict * 'a dict

let fruitd =
Node ("grape", 2.65,
Node ("banana", 1.50,
Node ("apple", 2.25, Empty, Empty),
Node ("cherry", 2.75, Empty, Empty)),
Node ("orange", 0.75,
Node ("kiwi", 3.99, Empty, Empty),
Node ("peach", 1.99, Empty, Empty)))
;;

let rec find d k =
        match d with
        | Empty -> raise (Foo k)
        | Node (k',v',l,r) ->
                        if k = k' then v' else
                        if k < k' then find l k else
                                find r k
;;

let rec add d k v =
        match d with
        | Empty -> Node (k,v,Empty,Empty)
        | Node (k',v',l,r) -> 
                        if k = k' then Node(k',v,l,r) else
                        if k < k' then Node(k',v',(add l k v),r) else
                                Node(k',v',l,(add r k v))
;;

let rec fold f b d =
        match d with
        | Empty -> b
        | Node (k,v,l,r) -> let left = fold f b l in
                let curr = f k v left in
                fold f curr r
;;
