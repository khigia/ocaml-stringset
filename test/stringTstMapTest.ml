(*
OUnit examples:
    OUnit.assert_bool "reason if failure" (x == 2)
    OUnit.assert_equal x y
    OUnit.assert_raises (Failure "index error") (fun () -> arr.[l]);
Tests examples:
    Tests.register "my feature" (fun () -> ... some code with OUnit assertions)
    Tests.run "Name of the suite"
*)

module TstMapTester = MapUtil.StringMapTester (StringSet.TstMap)

let _ = Tests.register "create and bind" TstMapTester.test_bind

let _ = Tests.register "bind-lookup" TstMapTester.test_lookup

let _ = Tests.run "TstMap test suite"

