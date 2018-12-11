let sum_matrix m = 
        let base = 0 in
        let fold_fn b lval = List.fold_left (+) b lval in
        List.fold_left fold_fn base m
;
