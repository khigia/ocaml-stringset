module type Costs = sig
  type v
  val insert: v -> float
  val delete: v -> float
  val replace: v -> v -> float
end

module type Indexable = sig
  type t
  type v
  val length: t -> int
  val get: t -> int -> v
end

module MakeArray (T:sig type t end) = struct
  type v = T.t
  type t = T.t array
  let length = Array.length
  let get = Array.get
end

module String = struct
  type t = string
  type v = char
  let length = String.length
  let get = String.get
end


module Edit (S:Indexable) (C:Costs with type v = S.v) = struct

  module Edition = struct
    type action =
      | Insert of int
      | Delete of int
      | Ident of int * int
      | Replace of int * int
    
    type t = Edit of S.t * S.t * action list
    
    let print ch d tostr =
      match d with Edit(a1, a2, d) ->
        List.iter (fun v -> match v with
          | Insert i -> Printf.fprintf ch "+ %s\n" (tostr (S.get a1 i))
          | Delete i -> Printf.fprintf ch "- %s\n" (tostr (S.get a2 i))
          | Ident (i, _) -> Printf.fprintf ch "  %s\n" (tostr (S.get a1 i))
          | Replace (i, j) ->
            let c1 = S.get a1 i in
            let c2 = S.get a2 j in
            Printf.fprintf ch "-+%s\n+-%s\n" (tostr c1) (tostr c2)
        ) d
  end
  module E = Edition

  type t = Mat of S.t * S.t * float array array

  let min3 a b c =
    min a (min b c)

  let make s1 s2 =
    let l1, l2 = S.length s1, S.length s2 in
    let m = Array.make_matrix (l1 + 1) (l2 + 1) 0.0 in
    for i = 0 to l1 do m.(i).(0) <- float i done;
    for j = 0 to l2 do m.(0).(j) <- float j done;
    for i = 0 to l1 - 1 do
      for j = 0 to l2 - 1 do
        let mi, mj = i + 1, j + 1 in
        let c1, c2 = S.get s1 i, S.get s2 j in
        let v1 = m.(mi).(mj-1) +. C.insert c2 in
        let v2 = m.(mi-1).(mj) +. C.delete c1 in
        let v3 = m.(mi-1).(mj-1) +. C.replace c1 c2 in
        m.(mi).(mj) <- min3 v1 v2 v3
      done
    done;
    Mat(s1, s2, m)

  let eprint = function Mat(s1, s2, m) ->
    let l1, l2 = S.length s1, S.length s2 in
    for j = 0 to l2 do
      for i = 0 to l1 do
        Printf.eprintf " %f" m.(i).(j)
      done;
      Printf.eprintf "\n"
    done

  let rec _read m s1 s2 i j r =
    match i, j with
    | 0, 0 ->
      E.Edit(s1, s2, r)
    | 0, _ ->
      _read m s1 s2 i (j-1) ((E.Delete (j-1))::r)
    | _, 0 ->
      _read m s1 s2 (i-1) j ((E.Insert (i-1))::r)
    | _, _ ->
      let c1, c2 = S.get s1 (i-1), S.get s2 (j-1) in
      (* hardcoded priority: insert, delete, replace *)
      let v1 = m.(i).(j-1) +. C.insert c2 in
      if m.(i).(j) = v1
      then
        _read m s1 s2 i (j-1) ((E.Delete (j-1))::r)
      else
        let v2 = m.(i-1).(j) +. C.delete c1 in
        if m.(i).(j) = v2
        then
          _read m s1 s2 (i-1) j ((E.Insert (i-1))::r)
        else
          (* the diff between Ident and Replace may not need to be therer but this help
          for further processing when the input c1 or c2 may be discarded. *)
          if c1 = c2
          then _read m s1 s2 (i-1) (j-1) ((E.Ident (i-1, j-1))::r)
          else _read m s1 s2 (i-1) (j-1) ((E.Replace (i-1, j-1))::r)

  let read = function Mat(s1, s2, m) ->
    let l1, l2 = S.length s1, S.length s2 in
    _read m s1 s2 l1 l2 []

  let align x y =
    let e = make x y in
    (*eprint e;*)
    read e
    
end
