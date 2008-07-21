(*
OUnit examples:
    OUnit.assert_bool "reason if failure" (x == 2)
    OUnit.assert_equal x y
    OUnit.assert_raises (Failure "index error") (fun () -> arr.[l]);
Tests examples:
    Tests.register "my feature" (fun () -> ... some code with OUnit assertions)
    Tests.run "Name of the suite"
*)

open StringSet
open Tst

let _ = Tests.register "create" (fun () ->
    let creator () =
        let _ = Tst.create () in
        true
    in
    OUnit.assert_bool "cannot create trie" (creator ())
)

let _ = Tests.register "insert" (fun () ->
    let ins () =
        let t0 = Tst.create () in
        OUnit.assert_equal t0 E;
        let t1 = Tst.insert t0 "ab" in
        OUnit.assert_equal t1 (N(E,N(E,E,E,'b'),E,'a'));
        let t2 = Tst.insert t1 "abc" in
        OUnit.assert_equal t2 (N(E,N(E,N(E,E,E,'c'),E,'b'),E,'a'));
        let t3 = Tst.insert t2 "ab" in
        OUnit.assert_equal t3 (N(E,N(E,N(E,E,E,'c'),E,'b'),E,'a'));
        let t4 = Tst.insert t3 "aaa" in
        OUnit.assert_equal t4 (N(E,N(N(E,N(E,E,E,'a'),E,'a'),N(E,E,E,'c'),E,'b'),E,'a'));
        true
    in
    OUnit.assert_bool "cannot insert" (ins ())
)

let _ = Tests.run "Tst test suite"

