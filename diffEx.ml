module MakeUnitCosts (T:sig type t end) = struct
  type v = T.t
  let insert v = 1.0
  let delete v = 1.1
  let replace v1 v2 = if v1 = v2 then 0.0 else (delete v1 +. insert v2)
end

module IntUnitCosts = MakeUnitCosts (struct type t = int end)
module CharUnitCosts = MakeUnitCosts (struct type t = char end)

module IntArray = Diff.MakeArray (struct type t = int end)

module IntArrayEdit = Diff.Edit (IntArray) (IntUnitCosts)

module StringEdit = Diff.Edit (Diff.String) (CharUnitCosts)

let test_arr =
    let x = [| 1; 2; 3; 4; 5; |] in
    let y = [| 8; 2; 4; 5; |] in
    let e = IntArrayEdit.make x y in
    IntArrayEdit.eprint e;
    let d = IntArrayEdit.read e in
    Printf.eprintf "Length of diff: %d\n" (List.length d);
    List.iter (fun e -> Printf.eprintf "%c" e) d;
    Printf.eprintf "\n"

let test_str =
    let x = "abcde" in
    let y = "wbde" in
    let e = StringEdit.make x y in
    StringEdit.eprint e;
    let d = StringEdit.read e in
    Printf.eprintf "Length of diff: %d\n" (List.length d);
    List.iter (fun e -> Printf.eprintf "%c" e) d;
    Printf.eprintf "\n"

let _ =
    test_arr ;
    test_str ;
    ()
