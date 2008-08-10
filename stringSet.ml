(*TODO

 - sort
 - range search
 - longest_prefix
 - wildcard match
 - neighrest neighbor search (k difference)

 - multi set (a set with list as value)
 
 - patricia trie/tree (crit bit tree)

 - ex: T9
*)

(* Notes
 - most of the ops not tail recursive (that could be done through continuations if needed)
*)

open StringUtil


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


module CTst = struct (* collapsed TST *)
    type 'a t =
        | E
        | N of 'a t * 'a t * 'a t * string
        (* semantic: N(l,m,r,"abc") = N(E,N(E,N(E,E,E,'c'),E,'b'),e,'a') *)

    let create () =
        E

    let rec _insert t s i len =
        if i = len then t
        else match t with
            | E ->
                let v = if i = 0 then s else String.sub s i (len - i) in
                N(E, E, E, v)
            | N(left, mid, right, c) as n ->
                let clen = String.length c in
                match string_cmp_i s i len c 0 clen with
                | Eq ->
                    n
                | Prefix (* s+i is prefix of c *) ->
                    n (* do not even need to mark the end of the key *)
                | Contain ->
                    N(left, _insert mid s (i + clen) len, right, c)
                | Inf p (* p < len *) ->
                    (* s and c may share a common prefix *)
                    if p = i
                    then (* empty prefix, s < c *)
                        N(_insert left s i len, mid, right, c)
                    else
                        let prefix = String.sub s i (p - i) in
                        let rest = String.sub c (p - i) (clen - p - i) in
                        let sub = _insert (N(left, mid, right, rest)) s (i + p) len in
                        N(E, sub, E, prefix)
                | Sup p ->
                    (* s and c may share a common prefix *)
                    if p = i
                    then (* empty prefix, s > c *)
                        N(left, mid, _insert right s i len, c)
                    else
                        let prefix = String.sub s i (p - i) in
                        let rest = String.sub c (p - i) (clen - p - i) in
                        let sub = _insert (N(left, mid, right, rest)) s (i + p) len in
                        N(E, sub, E, prefix)

    let insert t s =
        let len = String.length s in
        _insert t s 0 len

    let rec _has_prefix t s i len =
        if i = len then true
        else match t with
            | E ->
                false
            | N(l,m,r,c) ->
                let clen = String.length c in
                match string_cmp_i s i len c 0 clen with
                | Eq ->
                    true
                | Prefix (* s+i is prefix of c *) ->
                    true
                | Contain ->
                    _has_prefix m s (i + clen) len
                | Inf p (* p < len *) ->
                    if p = i then
                        _has_prefix l s i len
                    else
                        false
                | Sup p ->
                    if p = i then
                        _has_prefix r s i len
                    else
                        false

    let has_prefix t s =
        _has_prefix t s 0 (String.length s)

end (* module CTst *)


module Radix = struct
    (* not sure about the algo ... maybe a mix of patricia, radix, critbit *)

    type 'a _n_t = (* node: Leaf, Trunk (for prefix inputs), Branch *)
        | L of string * 'a
        | T of 'a _n_t * string * 'a
        | B of 'a _n_t * 'a _n_t * int * int (* left, right, critical char index, critical bit index (this is bit index in char ... from right to left) *)

    type 'a t = 'a _n_t option

    let create () =
        None

    let rec _first_key t =
        match t with
        | L(k, _) ->
            k
        | T(_, k, _) ->
            k
        | B(l, r, _, _) ->
            _first_key l (* favor left??? ... could take the shortest path if B node embed the size of tree *)
        
    let rec _bind t s sl v =
        match t with
        | L(k, d) ->
            begin
            let kl = String.length k in
            match string_cmp_i s 0 sl k 0 kl with
            | Eq ->
                L(k, v) (* replace *)
            | Prefix ->
                T(t, s, v)
            | Contain ->
                T(L(s, v), k, d)
            | Inf p ->
                let b = critbit s.[p] k.[p] in (* can raise but this would be unexpected here *)
                B(L(s, v), t, p, b)
            | Sup p ->
                let b = critbit s.[p] k.[p] in (* can raise but this would be unexpected here *)
                B(t, L(s, v), p, b)
            end
        | T(m, k, v) ->
            let kl = String.length k in
            if kl = sl then
                T(m, k, v) (* replace *)
            else
                _bind m s sl v
        | B(l, r, i, b) ->
            if sl > i then
                if ((int_of_char s.[i]) land (1 lsl b)) = 0 then
                    B(_bind l s sl v, r, i, b)
                else
                    B(l, _bind r s sl v, i, b)
            else
                begin
                let k = _first_key l in (* get the first string in tree (at least size i) *)
                match string_cmp_i s 0 sl k 0 sl with
                | Eq | Prefix ->
                    T(t, s, v)
                | Contain ->
                    failwith "unexpected data structure state (for real ... this is a bug!)"
                | Inf p ->
                    let bn = critbit s.[p] k.[p] in (* can raise but this would be unexpected here *)
                    B(L(s, v), t, p, bn)
                | Sup p ->
                    let bn = critbit s.[p] k.[p] in (* can raise but this would be unexpected here *)
                    B(t, L(s, v), p, bn)
                end

    let bind radix s v =
        match radix with
        | None -> Some(L(s, v))
        | Some t ->
            let sl = String.length s in
            Some(_bind t s sl v)

    let rec _lookup t s sl =
        match t with
        | L(k, v) ->
            if s = k then Some v else None
        | T(m, k, v) ->
            let kl = String.length k in
            if kl < sl then
                _lookup m s sl
            else if kl > sl then
                None
            else (* kl == sl *)
                (if s = k then Some v else None)
        | B(l, r, i, b) ->
            if sl > i then
                let dir = if ((int_of_char s.[i]) land (1 lsl b)) = 0 then l else r in
                _lookup dir s sl
            else
                None

    let lookup radix s =
        match radix with
        | None -> None
        | Some t ->
            let sl = String.length s in
            _lookup t s sl

end (* module Radix *)


module type Map = sig
    (* dict or map or finiteMap *)

    type key_t
    type 'a t

    val create: unit -> 'a t

    val lookup: 'a t -> key_t -> 'a option

    val bind: 'a t -> key_t -> 'a -> 'a t

end (* module type Map *)


module TstMap : Map with type key_t = string = struct

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


