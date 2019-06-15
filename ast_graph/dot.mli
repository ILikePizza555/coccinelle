module StringSet : Set.S with type elt = string

type node = {
    id: string;
    attrs: (string * string) list
}

val simple_node : string -> node

val string_of_node : node -> string

type edge = {
    source: string;
    sinks: StringSet.t
}

val string_of_edge : edge -> string

type graph = {
    name: string;
    nodes: (string, node) Hashtbl.t;
    edges: edge list;
}

val add_node : graph -> node -> graph

(* Adds a new edge connecting the two nodes specified by their ids*)
val connect : graph -> string -> string -> graph

val string_of_graph : graph -> string