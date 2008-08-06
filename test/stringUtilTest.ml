(*
OUnit examples:
    OUnit.assert_bool "reason if failure" (x == 2)
    OUnit.assert_equal x y
    OUnit.assert_raises (Failure "index error") (fun () -> arr.[l]);
Tests examples:
    Tests.register "my feature" (fun () -> ... some code with OUnit assertions)
    Tests.run "Name of the suite"
*)

open StringUtil


let _ = Tests.register "string_cmp_i eq" (fun () ->
    OUnit.assert_equal Eq (string_cmp_i "abc" 0 3 "abc" 0 3);
    OUnit.assert_equal Eq (string_cmp_i "abc" 0 3 "abcd" 0 3);
    OUnit.assert_equal Eq (string_cmp_i "abc" 1 3 "abc" 1 3);
    OUnit.assert_equal Eq (string_cmp_i "abc" 1 3 "zbc" 1 3);
    ()
)

let _ = Tests.register "string_cmp_i prefix" (fun () ->
    OUnit.assert_equal Prefix (string_cmp_i "abc" 0 3 "abcd" 0 4);
    OUnit.assert_equal Prefix (string_cmp_i "abc" 1 3 "abcd" 1 4);
    OUnit.assert_equal Prefix (string_cmp_i "abcd" 1 3 "abcd" 1 4);
    OUnit.assert_equal Prefix (string_cmp_i "abcd" 1 3 "zbcd" 1 4);
    ()
)

let _ = Tests.register "string_cmp_i contains" (fun () ->
    OUnit.assert_equal Contain (string_cmp_i "abc" 0 3 "ab" 0 2);
    OUnit.assert_equal Contain (string_cmp_i "abc" 1 3 "ab" 1 2);
    OUnit.assert_equal Contain (string_cmp_i "abcd" 1 4 "abc" 1 3);
    OUnit.assert_equal Contain (string_cmp_i "abcd" 1 4 "zbc" 1 3);
    ()
)

let _ = Tests.register "string_cmp_i inf" (fun () ->
    OUnit.assert_equal (Inf 2) (string_cmp_i "zbc" 0 3 "zbd" 0 3);
    OUnit.assert_equal (Inf 2) (string_cmp_i "zbc" 1 3 "zbd" 1 3);
    OUnit.assert_equal (Inf 2) (string_cmp_i "zbc" 1 3 "zbde" 1 4);
    OUnit.assert_equal (Inf 2) (string_cmp_i "zbc" 1 3 "abde" 1 4);
    ()
)

let _ = Tests.register "string_cmp_i sup" (fun () ->
    OUnit.assert_equal (Sup 2) (string_cmp_i "abd" 0 3 "abc" 0 3);
    OUnit.assert_equal (Sup 2) (string_cmp_i "abd" 1 3 "abc" 1 3);
    OUnit.assert_equal (Sup 2) (string_cmp_i "abd" 1 3 "abce" 1 4);
    OUnit.assert_equal (Sup 2) (string_cmp_i "abd" 1 3 "zbce" 1 4);
    ()
)

let _ = Tests.run "StringUtil test suite"


