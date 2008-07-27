(*
TODO
 - replace Empty node by Leaf node which contains a char (then cannot have empty Tst)
 - collapse the 1-branch paths (use string or list?)
*)
module Tst = struct
    type 'a t =
        | E
        | N of 'a t * 'a t * 'a t * char

    let create () =
        E

    let rec _insert t s i len =
        if i = len then t
        else match t with
            | E ->
                N(E, _insert E s (i + 1) len, E, s.[i])
            | N(left, mid, right, c) ->
                let cur = s.[i] in
                if cur < c then
                    N(_insert left s i len, mid, right, c)
                else if cur > c then
                    N(left, mid, _insert right s i len, c)
                else (*cur = c*)
                    N(left, _insert mid s (i + 1) len, right, c)

    let insert t s =
        let len = String.length s in
        _insert t s 0 len

    let rec _has_prefix t s i len =
        if i = len then true
        else match t with
            | E ->
                false
            | N(l,m,r,c) ->
                let cur = s.[i] in
                if cur < c then
                    _has_prefix l s i len
                else if cur > c then
                    _has_prefix r s i len
                else (*cur = c*)
                    _has_prefix m s (i + 1) len

    let has_prefix t s =
        _has_prefix t s 0 (String.length s)

end (* module Tst *)


module type Map = sig
    (* dict or map or finiteMap *)

    type key_t
    type 'a t

    val create: unit -> 'a t

    val lookup: 'a t -> key_t -> 'a option

    val bind: 'a t -> key_t -> 'a -> 'a t

end (* module type Map *)


module TstMap : Map with type key_t = string = struct
    (*
    TODO
     - replace Empty node by Leaf node which contains a char (then cannot have empty Tst)
     - collapse the 1-branch paths (use string or list?)
    *)

    type key_t = string

    type 'a t =
        | E
        | N of 'a t * 'a t * 'a t * char * 'a option

    let create () =
        E

    let rec _bind t s i len v =
        if i = len - 1 then
            begin
            match t with
            | E ->
                N(E, E, E, s.[i], Some v)
            | N(l, m, r, c, o) ->
                let cur = s.[i] in
                if cur < c then
                    N(_bind l s i len v, m, r, c, o)
                else if cur > c then
                    N(l, m, _bind r s i len v, c, o)
                else (*cur = c*)
                    N(l, m, r, c, Some v) (* REPLACE existing value *)
            end
        else match t with
            | E ->
                N(E, _bind E s (i + 1) len v, E, s.[i], None)
            | N(left, mid, right, c, o) ->
                let cur = s.[i] in
                if cur < c then
                    N(_bind left s i len v, mid, right, c, o)
                else if cur > c then
                    N(left, mid, _bind right s i len v, c, o)
                else (*cur = c*)
                    N(left, _bind mid s (i + 1) len v, right, c, o)

    let bind t s v =
        let len = String.length s in
        if len > 0 then _bind t s 0 len v
        else t

    let rec _lookup t s i len =
        if i = len - 1 then
            begin
            match t with
            | E ->
                None
            | N(l, m, r, c, o) ->
                let cur = s.[i] in
                if cur < c then
                    _lookup l s i len
                else if cur > c then
                    _lookup r s i len
                else (*cur = c*)
                    o
            end
        else match t with
            | E ->
                None
            | N(l,m,r,c,_) ->
                let cur = s.[i] in
                if cur < c then
                    _lookup l s i len
                else if cur > c then
                    _lookup r s i len
                else (*cur = c*)
                    _lookup m s (i + 1) len

    let lookup t s =
        let len = String.length s in
        if len > 0 then _lookup t s 0 len
        else None

end (* module TstMap *)


(* adapted from "Purely functional data structure" [Chris Okasaki, 1998]
bootstraped a Map of char to create a Map of string
*)
module TrieFtor (M : Map with type key_t = char) : Map with type key_t = string = struct

    type key_t = string

    type 'a t = Trie of 'a option * 'a t M.t

    let create () = Trie(None, M.create ())

    let rec _lookup t s i len =
        match t with
        | Trie(maybe, m) ->
            if i = len then
                maybe
            else
                begin
                match M.lookup m s.[i] with
                | Some trie ->
                    _lookup trie s (i + 1) len
                | None ->
                    None
                end
    
    let lookup t s =
        _lookup t s 0 (String.length s)

    let rec _bind t s i len v =
        match t with
        | Trie(o, m) ->
            begin
            if i = len then Trie(Some v, m) (* discard o *)
            else
                begin
                let trie = match M.lookup m s.[i] with
                | Some mt ->
                    mt
                | None ->
                    create ()
                in
                let tt = _bind trie s (i + 1) len v in
                Trie(o, M.bind m s.[i] tt)
                end
            end

    let bind t s v =
        _bind t s 0 (String.length s) v
end

(* Build a Trie based on a Hashtbl *)
module HashtblTrie = TrieFtor (struct
    (* Need to wrap Hastbl to match our Map definition *)

    type key_t = char
    type 'a t = (char, 'a) Hashtbl.t

    let create () =
        Hashtbl.create 128

    let bind t s v =
        let _ = Hashtbl.replace t s v in
        t

    let lookup t s =
        try let v = Hashtbl.find t s in Some v
        with Not_found -> None
end)


