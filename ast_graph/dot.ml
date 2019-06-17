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

(* Main record type for a graph. 

Note that a nodes in our graph don't strictly have to exist in nodes in order
to be viewed, thanks to how DOT works. It is enough to simple declare the name
in the edges. This is why `connect` doesn't check for existance in nodes. 
The nodes table exists mainly for applying attributes to nodes on the graph. 
 *)
type graph = {
    name: string;
    nodes: (string, node) Hashtbl.t;
    edges: (string, edge) Hashtbl.t
}

let add_node g n =
    Hashtbl.add g.nodes n.id n

let connect g n_id1 n_id2 =
    if Hashtbl.mem g.edges n_id1 
    then (Hashtbl.find g.edges n_id1) |> (add_sink n_id2) |> (Hashtbl.replace g.edges n_id1)
    else Hashtbl.add g.edges n_id1 (make_edge n_id1 n_id2)

let string_of_graph g =
    let tab_concat sf = fun _ v acc -> acc ^ "\t" ^ (sf v) in
    "digraph " ^ g.name ^ "{ \n" ^
    Hashtbl.fold (tab_concat string_of_node) g.nodes "" ^
    Hashtbl.fold (tab_concat string_of_edge) g.edges "" ^
    "}"

(* Functions for converting ASTs to graphs.*)

let graph_from_c_ast ast graph_name =
    let initial_table_size = 1000 in
    let g : graph = {
        name = graph_name; 
        nodes = Hashtbl.create initial_table_size;
        edges = Hashtbl.create initial_table_size;} in
    let open Visitor_c in
    let vistor : Visitor_c.visitor_c = {
        kexpr           = (fun (next, self) expr -> ());
        kassignOp       = (fun (next, self) op -> ());
        kbinaryOp       = (fun (next, self) op -> ());
        kstatement      = (fun (next, self) stmt -> ());
        ktype           = (fun (next, self) t -> ());
        kdecl           = (fun (next, self) d  -> ());
        konedecl        = (fun (next, self) d  -> ());
        konedecl_opt    = (fun _ (next, self) d  -> ());
        kparam          = (fun (next, self) d  -> ());
        kdef            = (fun (next, self) d  -> ());
        kini            = (fun (next, self) ie  ->());
        kname           = (fun (next, self) x -> ());
        kfragment       = (fun (next, self) f  -> ());
        kformat         = (fun (next, self) f  -> ());
        kinfo           = (fun (next, self) ii  ->());
        knode           = (fun (next, self) n  -> ());
        ktoplevel       = (fun (next, self) p  -> ());
        kcppdirective   = (fun (next, self) p  -> ());
        kifdefdirective = (fun (next, self) p  -> ());
        kdefineval      = (fun (next, self) p  -> ());
        kstatementseq   = (fun (next, self) p  -> ());
        kfield          = (fun (next, self) p  -> ());
    } in
    Visitor_c.vk_program vistor ast