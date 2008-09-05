(*
Diff files:
- for line mode:
  - may apply transfo on line: s/\s+/ /g
  - using stringset, tag each line to an int identifier; align those int
  - display result might want to perform align of the line for replace edition
- word mode
- for char mode, we may still want to perform some transfo (ignore newline ...)
- for byte mode, no transfo?
- skip head while ident (possible to skip common tail?)
- for big file, may need to create non-optimal alignment from succession of local align (every 100 line or 100 bytes)
*)

let (|>) x f = f x

module MakeCosts (T:sig type t end) = struct
  type v = T.t
  let insert v = 1.0
  let delete v = 1.0
  let replace v1 v2 = if v1 = v2 then 0.0 else (delete v1 +. insert v2)
end

module IntArray = Diff.MakeArray (struct type t = int end)
module IntCosts = MakeCosts (struct type t = int end)
module IntDiff = Diff.Edit (IntArray) (IntCosts)

module StringArray = Diff.MakeArray (struct type t = string end)
module StringCosts = MakeCosts (struct type t = string end)
module StringDiff = Diff.Edit (StringArray) (StringCosts)


type lines = Lines of string * string array

let rec read_lines ch acc =
  try
    let line = input_line ch in
    read_lines ch (line::acc)
  with End_of_file -> acc

let file_lines_to_sequence fn =
  let ch = open_in fn in
  let lines = read_lines ch [] in
  Lines(fn, Array.of_list (List.rev lines))

let rec _do_diff_line s0 files =
  match files with
  | fn::rest ->
    let s = file_lines_to_sequence fn in
    let Lines(fn0, a0) = s0 in
    let Lines(fn, a) = s in
    Printf.eprintf "Compare %s and %s\n" fn0 fn;
    let d = StringDiff.align a0 a in
    StringDiff.Edition.print stderr d (fun s -> s);
    _do_diff_line s0 rest
  | _ -> ()

let do_diff_line fn0 files =
  (* line to int using a radix/CTST map ... save memory and cpu! *)
  let s0 = file_lines_to_sequence fn0 in
  _do_diff_line s0 files



let die msg code =
  Printf.eprintf "%s\n" msg;
  exit code

let check_file fn =
  if not (Sys.file_exists fn) then die (Printf.sprintf "File not found: %s" fn) 20;
  fn

let usage () =
  Printf.sprintf "Usage: %s file0 file1 file2 ...\nCreate a diff of file0 with each of following files.\n" (Filename.basename Sys.executable_name)


type mode = Line

let _ =
  let fns = ref [] in
  let mode = ref Line in
  let _ = Arg.parse
    [
      ("-l", Arg.Unit (fun () -> mode := Line), "Diff by line.");
    ]
    (fun fn -> fns := (check_file fn) :: !fns)
    (usage ())
  in
  match List.rev !fns with
  | fn0::fn1::files ->
    begin
    match !mode with
    | Line -> do_diff_line fn0 (fn1::files)
    end
  | _ -> die (usage ()) 1


