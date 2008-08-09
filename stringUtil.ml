type string_cmp_i_t =
    | Eq
    | Prefix
    | Contain
    | Inf of int
    | Sup of int

let rec string_cmp_i s1 p1 l1 s2 p2 l2 =
    if p1 = l1 then
        begin
        if p2 = l2 then
            Eq
        else
            Prefix
        end
    else
        begin
        if p2 = l2 then
            Contain
        else
            let c1 = s1.[p1] in
            let c2 = s2.[p2] in
            if c1 = c2 then
                string_cmp_i s1 (p1 + 1) l1 s2 (p2 + 1) l2
            else if c1 < c2 then
                Inf p1
            else (* c1 > c2 *)
                Sup p1
        end

let rec _critbit p c1 c2 =
    if (c1 land 128) != (c2 land 128) then
        p
    else
        _critbit (p - 1) (c1 lsl 1) (c2 lsl 1)

let critbit c1 c2 =
    if c1 = c2 then
        raise Not_found
    else
        (* TODO a XOR and find first bit should be more efficient *)
        _critbit 7 (int_of_char c1) (int_of_char c2)
