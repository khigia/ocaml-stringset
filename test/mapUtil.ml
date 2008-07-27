open StringSet

module StringMapTester (M : Map with type key_t = string) = struct

    let test_bind () =
        let t0 = M.create () in
        let t1 = M.bind t0 "abc" "ABC" in
        let t2 = M.bind t1 "abc" "ABC2" in
        let t3 = M.bind t2 "" "any" in
        let _ = M.bind t3 "z" "Z" in
        OUnit.assert_bool "all bind ok" true

    let test_lookup () =
        let t0 = M.create () in
        OUnit.assert_equal None (M.lookup t0 "ab");
        let t1 = M.bind t0 "ab" "AB" in
        OUnit.assert_equal (Some "AB") (M.lookup t1 "ab");
        let t2 = M.bind t1 "abc" "ABC" in
        OUnit.assert_equal (Some "ABC") (M.lookup t2 "abc");
        let t3 = M.bind t2 "abc" "ABC2" in
        OUnit.assert_equal (Some "ABC2") (M.lookup t3 "abc");
        let t4 = M.bind t3 "a" "A" in
        OUnit.assert_equal (Some "A") (M.lookup t4 "a");
        OUnit.assert_equal (Some "ABC2") (M.lookup t4 "abc");
        let t5 = M.bind t4 "z" "Z" in
        OUnit.assert_equal (Some "Z") (M.lookup t5 "z");
        ()

end
