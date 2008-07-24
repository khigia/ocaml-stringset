module Tst = struct
    type 'a t =
        | E (* can have a char to avoid node with 3 empty nodes *)
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
        _insert t s 0 (String.length s)

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
