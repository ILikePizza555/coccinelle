module StringSet = Set.Make(String)

type node = {
    id: string;
    attrs: (string * string) list
}

let simple_node id = {id = id; attrs = []}

let string_of_node n = 
    "\"" ^ n.id ^ "\# [" ^
    List.fold_left (fun acc (k, v) -> acc ^ k ^ "=" ^ v ^ " ") "" n.attrs
    ^ "]\n"

type edge = {
    source: string;
    sinks: StringSet.t
}