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


