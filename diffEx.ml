module UnitCosts = struct
  type v = int
  let insert v = 1.0
  let delete v = 1.0
  let replace v1 v2 = if v1 = v2 then 0.0 else 2.0
end

module IntArray = Diff.MakeArray (struct type t = int end)

module SimpleEdit = Diff.Edit (IntArray) (UnitCosts)

let _ =
    let x = [| 1; 2; 3; 4; 5; |] in
    let y = [| 8; 2; 4; 5; |] in
    let e = SimpleEdit.make x y in
    SimpleEdit.eprint e;
    let d = SimpleEdit.read e in
    Printf.eprintf "Length of diff: %d\n" (List.length d);
    List.iter (fun e -> Printf.eprintf "%c" e) d;
    Printf.eprintf "\n"
