module MakeCosts (T:sig type t end) = struct
  type v = T.t
  let insert v = 1.0
  let delete v = 1.1
  let replace v1 v2 = if v1 = v2 then 0.0 else (delete v1 +. insert v2)
end


let test_arr =
  let module IntArray = Diff.MakeArray (struct type t = int end) in
  let module IntCosts = MakeCosts (struct type t = int end) in
  let module M = Diff.Edit (IntArray) (IntCosts) in
  let x = [| 1; 2; 3; 4; 5; |] in
  let y = [| 8; 2; 4; 5; |] in
  let d = M.align x y in
  M.Edition.print stderr d string_of_int

let test_str =
  let module CharCosts = MakeCosts (struct type t = char end) in
  let module M = Diff.Edit (Diff.String) (CharCosts) in
  let x = "abcde" in
  let y = "wbde" in
  let d = M.align x y in
  M.Edition.print stderr d (fun c -> String.make 1 c)

let test_stra =
  let module StringArray = Diff.MakeArray (struct type t = string end) in
  let module StringCosts = MakeCosts (struct type t = string end) in
  let module M = Diff.Edit (StringArray) (StringCosts) in
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
  let d = M.align x y in
  M.Edition.print stderr d (fun s -> s)

let _ =
  test_arr ;
  test_str ;
  test_stra ;
  ()
