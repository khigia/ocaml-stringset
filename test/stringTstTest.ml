(*
OUnit examples:
    OUnit.assert_bool "reason if failure" (x == 2)
    OUnit.assert_equal x y
    OUnit.assert_raises (Failure "index error") (fun () -> arr.[l]);
Tests examples:
    Tests.register "my feature" (fun () -> ... some code with OUnit assertions)
    Tests.run "Name of the suite"
*)

let assert_true reason test =
    OUnit.assert_bool reason test

let assert_false reason test =
    OUnit.assert_bool reason (not test)

open StringSet
open Tst

let _ab = N(E,N(E,E,E,'b'),E,'a')
let _abc = N(E,N(E,N(E,E,E,'c'),E,'b'),E,'a')
let _abc_aaa = N(E,N(N(E,N(E,E,E,'a'),E,'a'),N(E,E,E,'c'),E,'b'),E,'a')

let _ = Tests.register "create" (fun () ->
    let t0 = Tst.create () in
    OUnit.assert_equal t0 E
)

let _ = Tests.register "insert" (fun () ->
    let t0 = Tst.create () in
    OUnit.assert_equal t0 E;
    let t1 = Tst.insert t0 "ab" in
    OUnit.assert_equal t1 _ab;
    let t2 = Tst.insert t1 "abc" in
    OUnit.assert_equal t2 _abc;
    let t3 = Tst.insert t2 "ab" in
    OUnit.assert_equal t3 _abc;
    let t4 = Tst.insert t3 "aaa" in
    OUnit.assert_equal t4 _abc_aaa
)

let _ = Tests.register "has_prefix" (fun () ->
    assert_true "a should be prefix of ab" (Tst.has_prefix _ab "a");
    assert_true "ab should be prefix of ab" (Tst.has_prefix _ab "ab");
    assert_false "abc should not be prefix of ab" (Tst.has_prefix _ab "abc");
    assert_false "c should not be prefix of ab" (Tst.has_prefix _ab "c");
    assert_true "a should be prefix of abc-aaa" (Tst.has_prefix _abc_aaa "a");
    assert_true "aa should be prefix of abc-aaa" (Tst.has_prefix _abc_aaa "aa");
    assert_true "ab should be prefix of abc-aaa" (Tst.has_prefix _abc_aaa "ab");
    assert_true "abc should be prefix of abc-aaa" (Tst.has_prefix _abc_aaa "abc");
    assert_false "b should not be prefix of abc-aaa" (Tst.has_prefix _abc_aaa "b");
    ()
)

let _ = Tests.run "Tst test suite"

