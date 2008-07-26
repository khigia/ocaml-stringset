(*
OUnit examples:
    OUnit.assert_bool "reason if failure" (x == 2)
    OUnit.assert_equal x y
    OUnit.assert_raises (Failure "index error") (fun () -> arr.[l]);
Tests examples:
    Tests.register "my feature" (fun () -> ... some code with OUnit assertions)
    Tests.run "Name of the suite"
*)

open OUnit

open StringSet.TstMap


let _ = Tests.register "create and bind" (fun () ->
    let t0 = create () in
    let t1 = bind t0 "abc" "ABC" in
    let t2 = bind t1 "abc" "ABC2" in
    let t3 = bind t2 "" "any" in
    let _ = bind t3 "z" "Z" in
    OUnit.assert_bool "all bind ok" true
)

let _ = Tests.register "bind-lookup" (fun () ->
    let t0 = create () in
    OUnit.assert_equal None (lookup t0 "ab");
    let t1 = bind t0 "ab" "AB" in
    OUnit.assert_equal (Some "AB") (lookup t1 "ab");
    let t2 = bind t1 "abc" "ABC" in
    OUnit.assert_equal (Some "ABC") (lookup t2 "abc");
    let t3 = bind t2 "abc" "ABC2" in
    OUnit.assert_equal (Some "ABC2") (lookup t3 "abc");
    let t4 = bind t3 "a" "A" in
    OUnit.assert_equal (Some "A") (lookup t4 "a");
    OUnit.assert_equal (Some "ABC2") (lookup t4 "abc");
    let t5 = bind t4 "z" "Z" in
    OUnit.assert_equal (Some "Z") (lookup t5 "z");
    ()
)

let _ = Tests.run "TstMap test suite"

