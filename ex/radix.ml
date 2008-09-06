open Printf

module SRadix = struct
    include StringSet.Radix

    let debug t =
        let rec _debug t indent =
            match t with
            | L(k, v) ->
                eprintf "L:%s:%s\n" k v
            | T(n, k, v) ->
                eprintf "T:%s:%s\n" k v;
                eprintf "%s" (String.make indent ' ');
                _debug n indent
            | B(l, r, i, b) ->
                begin
                eprintf "B(%d, %d)\n" i b;
                eprintf "%s" (String.make indent ' '); eprintf "l:";
                _debug l (indent + 2);
                eprintf "%s" (String.make indent ' '); eprintf "r:";
                _debug r (indent + 2)
                end
        in
        match t with
        | None ->
            eprintf "empty\n"
        | Some v ->
            _debug v 0

end (* module radix *)


let _ =
    let d = List.fold_left (fun t (s, v) ->
        eprintf "Inserting: %s:%s\n" s v;
        let r = SRadix.bind t s v in
        SRadix.debug r;
        eprintf "\n";
        r
    ) (SRadix.create ()) [
        ("aaabc", "1") ;
        ("aaabb", "2") ;
        ("aaabddd", "3") ;
        ("azz", "4") ;
        ("aaab", "5") ;
        ("a", "6") ;
    ] in
    List.iter (fun k ->
        eprintf "Lookup %s: %s\n" k (match SRadix.lookup d k with
        | None -> "None"
        | Some v -> sprintf "Some %s" v
        ))
        [
            "a";
            "aaab";
            "aaabc" ;
            "aaabb" ;
            "aaabddd" ;
            "azz" ;
            "w";
            "abab";
            "aaabcd" ;
            "aaabd" ;
            "aaabddc" ;
            "azf" ;
        ]
