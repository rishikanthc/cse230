exception Notfound of string

let prices = [ ("Baseball Bat", 20); ("Soccer Ball", 10); ("Tennis Racket", 40) ];;

let rec find d k =
        match d with
        | [] -> raise (Notfound k)
        | (k',v')::t -> if k = k' then v' else
                if k' < k then find t k else
                        raise (Notfound k)
;;

let rec add d k v =
        match d with
        | [] -> [(k,v)]
        |(k',v')::t -> if k = k' then (k,v)::t else
                if k < k' then (k,v)::d else
                        (k',v')::add t k v
;;

let keys d = List.map (fun (k,v) -> k) d;;
let values d = List.map (fun (k,v) -> v) d;;

let key_of_max_val d = 
        let fold_fn (maxk, maxv) (lk,lv) = match maxv with
        | None -> (lk, Some lv)
        | Some a -> if lv > a then (lk,Some lv) else (maxk, maxv)
        in match d with
        |[] -> raise (Notfound "empty")
        |base::t -> let (maxk,_) = List.fold_left fold_fn ("",None) d in 
        maxk
;;
