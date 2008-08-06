(*
OUnit examples:
    OUnit.assert_bool "reason if failure" (x == 2)
    OUnit.assert_equal x y
    OUnit.assert_raises (Failure "index error") (fun () -> arr.[l]);
Tests examples:
    Tests.register "my feature" (fun () -> ... some code with OUnit assertions)
    Tests.run "Name of the suite"
*)

module HashtblTrieTester = MapUtil.StringMapTester (StringSet.HashtblTrie)

let _ = Tests.register "create and bind" HashtblTrieTester.test_bind

let _ = Tests.register "bind-lookup" HashtblTrieTester.test_lookup

let _ = Tests.run "HashtblTrie test suite"


