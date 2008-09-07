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

module LineDiff = struct
  module D = IntDiff

  module Printer = struct
    open D.Edition
  
    let rec read_line_n ch pos n =
      if n < pos then raise Not_found; (* should never happen. *)
      let l = input_line ch in
      let pos' = pos + 1 in
      if n = pos then l,pos' else read_line_n ch pos' n
  
    let rec _fprint ch ch1 p1 ch2 p2 edition =
      match edition with
      | [] -> ()
      | hd::tl ->
        match hd with
        | Ident i ->
          let l1, p1' = read_line_n ch1 p1 i in
          Printf.fprintf ch "%5d= %s\n" i l1;
          _fprint ch ch1 p1' ch2 p2 tl
        | Insert i ->
          let l1, p1' = read_line_n ch1 p1 i in
          Printf.fprintf ch "%5d+ %s\n" i l1;
          _fprint ch ch1 p1' ch2 p2 tl
        | Delete j ->
          let l2, p2' = read_line_n ch2 p2 j in
          Printf.fprintf ch "%5d- %s\n" j l2;
          _fprint ch ch1 p1 ch2 p2' tl
        | Replace (i, j) ->
          let l1, p1' = read_line_n ch1 p1 i in
          let l2, p2' = read_line_n ch2 p2 j in
          Printf.fprintf ch "%5d-*%s\n%5d+*%s\n" i l1 j l2;
          _fprint ch ch1 p1' ch2 p2' tl
  
    let fprint ch fn1 fn2 edition =
      let ch1, ch2 = open_in fn1, open_in fn2 in
      match edition with Edit(_,_,d) -> _fprint ch ch1 0 ch2 0 d
  
  end
  
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
    close_in ch;
    tagger, Array.of_list lines
  
  let diff fn0 files =
    let tagger = Tagger.create () in
    let tagger, a0 = file_lines_to_sequence tagger fn0 in
    List.iter (fun fn ->
      let _, a = file_lines_to_sequence tagger fn in
      Printf.eprintf "Compare %s and %s\n" fn0 fn;
      let d = D.align a0 a in
      (*D.Edition.print stderr d (fun s -> string_of_int s);*)
      Printer.fprint stderr fn0 fn d
    ) files

end


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
    | Line -> LineDiff.diff fn0 (fn1::files)
    end
  | _ -> die (usage ()) 1


