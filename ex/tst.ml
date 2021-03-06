open Printf

module Tst = struct
    include StringSet.Tst

    let debug t =
        let rec _debug t indent =
            match t with
            | E ->
                eprintf "empty\n"
            | N(l,m,r,c) ->
                begin
                eprintf "_:%c\n" c;
                eprintf "%s" (String.make indent ' '); eprintf "l:";
                _debug l (indent + 2);
                eprintf "%s" (String.make indent ' '); eprintf "m:";
                _debug m (indent + 2);
                eprintf "%s" (String.make indent ' '); eprintf "r:";
                _debug r (indent + 2)
                end
        in
        _debug t 0
end (* module Tst *)

let _ =
    let t0 = Tst.create () in
    let t1 = Tst.insert t0 "abc" in
    let t2 = Tst.insert t1 "abb" in
    let t3 = Tst.insert t2 "abd" in
    let t4 = Tst.insert t3 "zz" in
    Tst.debug t4;
    eprintf "Tst.has_prefix ab: %b\n" (Tst.has_prefix t4 "ab");
    eprintf "Tst.has_prefix abc: %b\n" (Tst.has_prefix t4 "abc");
    eprintf "Tst.has_prefix abf: %b\n" (Tst.has_prefix t4 "abf");
    eprintf "Tst.has_prefix a: %b\n" (Tst.has_prefix t4 "a");
    eprintf "Tst.has_prefix h: %b\n" (Tst.has_prefix t4 "h");
    ()
