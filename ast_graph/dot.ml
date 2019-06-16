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

exception InvalidEdge of string

let make_edge source sink = {
    source = source;
    sinks = StringSet.singleton sink
}

let string_of_edge e = 
    if StringSet.cardinal e.sinks == 0
        then raise (InvalidEdge "encoutered 0 sinks")
    else 
        e.source ^ "->" ^ (
            if StringSet.cardinal e.sinks == 1
            then StringSet.choose e.sinks
            else "{" ^ 
                (StringSet.fold (fun a c -> a ^ " \"" ^ c ^ "\"") e.sinks "") ^ 
            "}\n")

let add_sink sink e = { e with sinks = StringSet.add sink e.sinks }