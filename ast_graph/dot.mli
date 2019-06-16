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

val add_sink : edge -> string -> edge

type graph = {
    name: string;
    nodes: (string, node) Hashtbl.t;
    edges: edge list;
}

val add_node : graph -> node -> unit

(* Adds a new edge connecting the two nodes specified by their ids*)
val connect : graph -> string -> string -> unit

val string_of_graph : graph -> string