module MakeUnitCosts (T:sig type t end) = struct
  type v = T.t
  let insert v = 1.0
  let delete v = 1.1
  let replace v1 v2 = if v1 = v2 then 0.0 else (delete v1 +. insert v2)
end

module IntUnitCosts = MakeUnitCosts (struct type t = int end)
module CharUnitCosts = MakeUnitCosts (struct type t = char end)
module StringUnitCosts = MakeUnitCosts (struct type t = string end)

module IntArray = Diff.MakeArray (struct type t = int end)
module StringArray = Diff.MakeArray (struct type t = string end)

module IntArrayEdit = Diff.Edit (IntArray) (IntUnitCosts)
module StringArrayEdit = Diff.Edit (StringArray) (StringUnitCosts)

module StringEdit = Diff.Edit (Diff.String) (CharUnitCosts)


let test_arr =
  let x = [| 1; 2; 3; 4; 5; |] in
  let y = [| 8; 2; 4; 5; |] in
  let e = IntArrayEdit.make x y in
  IntArrayEdit.eprint e;
  let d = IntArrayEdit.read e in
  IntArrayEdit.eprint_edition e d (fun s i -> string_of_int s.(i))

let test_str =
  let x = "abcde" in
  let y = "wbde" in
  let e = StringEdit.make x y in
  StringEdit.eprint e;
  let d = StringEdit.read e in
  StringEdit.eprint_edition e d  (fun s i -> String.make 1 s.[i])

let test_stra =
  let x = [|
    "line 1";
    "line 2";
    "line 3";
    "line 4";
    "line 5";
  |] in
  let y = [|
    "new line 1";
    "line 2";
    "line 4";
    "line 5";
  |] in
  let e = StringArrayEdit.make x y in
  StringArrayEdit.eprint e;
  let d = StringArrayEdit.read e in
  StringArrayEdit.eprint_edition e d (fun s i -> s.(i))

let _ =
  test_arr ;
  test_str ;
  test_stra ;
  ()
