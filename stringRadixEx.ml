open Printf

module Radix = struct
    include StringSet.Radix

    let debug t =
        let rec _debug t indent =
            match t with
            | L(k) ->
                eprintf "L:%s\n" k
            | T(n, k) ->
                eprintf "T:%s\n" k;
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
    let _ = List.fold_left (fun t s ->
        eprintf "Inserting: %s\n" s;
        let r = Radix.insert t s in
        Radix.debug r;
        eprintf "\n";
        r
    ) (Radix.create ()) [
      "aaabc" ;
      "aaabb" ;
      "aaabddd" ;
      "azz" ;
      "aaab" ;
      "a" ;
    ] in
    ()
