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
open CTst

let tree () =
    let t0 = create () in
    let t1 = insert t0 "aaabc" in
    let t2 = insert t1 "aaabb" in
    let t3 = insert t2 "aaabddd" in
    let t4 = insert t3 "azz" in
    t4

let _ = Tests.register "create/insert" (fun () ->
    let _ = tree () in
    OUnit.assert_equal (create ()) E
)

let chk_true t s =
    assert_true (Printf.sprintf "%s should be prefix" s) (has_prefix t s)
    
let chk_false t s =
    assert_false (Printf.sprintf "%s should not be prefix" s) (has_prefix t s)
    
let _ = Tests.register "has_prefix" (fun () ->
    let t = tree () in
    chk_true t "a";
    chk_true t "aa";
    chk_true t "aaab";
    chk_true t "aaabc";
    chk_false t "aaabcd";
    chk_true t "aaabd";
    chk_true t "aaabdd";
    chk_false t "aaabdddd";
    chk_true t "az";
    chk_true t "azz";
    ()
)

let _ = Tests.run "Tst test suite"


