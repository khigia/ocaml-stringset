open Printf

module CTst = struct
    include StringSet.CTst

    let debug t =
        let rec _debug t indent =
            match t with
            | E ->
                eprintf "empty\n"
            | N(l,m,r,s) ->
                begin
                eprintf "_:%s\n" s;
                eprintf "%s" (String.make indent ' '); eprintf "l:";
                _debug l (indent + 2);
                eprintf "%s" (String.make indent ' '); eprintf "m:";
                _debug m (indent + 2);
                eprintf "%s" (String.make indent ' '); eprintf "r:";
                _debug r (indent + 2)
                end
        in
        _debug t 0
end (* module CTst *)

let ins t s =
    eprintf "Inserting: %s\n" s;
    let r = CTst.insert t s in
    CTst.debug r;
    eprintf "\n";
    r

let chk t s =
    eprintf "Prefix %s: %b\n" s (CTst.has_prefix t s)

let _ =
    let t0 = CTst.create () in
    let t1 = ins t0 "aaabc" in
    let t2 = ins t1 "aaabb" in
    let t3 = ins t2 "aaabddd" in
    let t4 = ins t3 "azz" in
    chk t4 "a";
    chk t4 "aa";
    chk t4 "aaab";
    chk t4 "aaabc";
    chk t4 "aaabcd";
    chk t4 "aaabd";
    chk t4 "aaabdd";
    chk t4 "aaabdddd";
    chk t4 "az";
    chk t4 "azz";
    ()
