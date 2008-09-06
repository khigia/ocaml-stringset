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

module Tagger = struct
  module Dict = StringSet.Radix

  type 'a t = {
    cur: int;
    dict: 'a Dict.t;
  }

  let create () = {
    cur = 0;
    dict = Dict.create ();
  }

  let get_tag st key =
    match Dict.lookup st.dict key with
    | Some v -> v, st
    | None ->
      let d = Dict.bind st.dict key st.cur in
      st.cur, {cur = st.cur + 1; dict = d; }
end

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


type lines = Lines of string * int array

let rec read_lines tagger ch acc =
  try
    let line = input_line ch in
    (* TODO line transfo happen now ... normalize space etc *)
    let tag, tagger = Tagger.get_tag tagger line in
    read_lines tagger ch (tag::acc)
  with End_of_file -> tagger, List.rev acc

let file_lines_to_sequence tagger fn =
  let ch = open_in fn in
  let tagger, lines = read_lines tagger ch [] in
  tagger, Lines(fn, Array.of_list lines)

let rec _do_diff_line tagger s0 files =
  match files with
  | fn::rest ->
    let _, s = file_lines_to_sequence tagger fn in
    let Lines(fn0, a0) = s0 in
    let Lines(fn, a) = s in
    Printf.eprintf "Compare %s and %s\n" fn0 fn;
    let d = IntDiff.align a0 a in
    IntDiff.Edition.print stderr d (fun s -> string_of_int s);
    _do_diff_line tagger s0 rest
  | _ -> ()

let do_diff_line fn0 files =
  let tagger = Tagger.create () in
  let tagger, s0 = file_lines_to_sequence tagger fn0 in
  _do_diff_line tagger s0 files



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


