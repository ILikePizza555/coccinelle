open Common
module StringSet = Set.Make(String)

type node = {
    id: string;
    attrs: (string * string) list
}

let simple_node id = {id = id; attrs = []}

let string_of_node n =
    let attr_str = List.fold_left (fun acc (k, v) ->
        acc ^ (Printf.sprintf "%s=%s" k v)) "" n.attrs in
    Printf.sprintf "\"%s\"[%s]" n.id attr_str

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
    let open Ast_c in
    let vistor : Visitor_c.visitor_c = {
        kexpr           = (fun (next, self) expr -> 
            add_node g (simple_node Ast_name.name_from_expr expr);
            next expr
        );
        kassignOp       = (fun (next, self) op -> next op);
        kbinaryOp       = (fun (next, self) op -> next op);
        kstatement      = (fun (next, self) stmt -> next stmt);
        ktype           = (fun (next, self) t -> next t);
        kdecl           = (fun (next, self) d  -> next d);
        konedecl        = (fun (next, self) d  -> next d);
        konedecl_opt    = (fun _ (next, self) d  -> next d);
        kparam          = (fun (next, self) d  -> next d);
        kdef            = (fun (next, self) d  -> next d);
        kini            = (fun (next, self) ie  -> next ie);
        kname           = (fun (next, self) x -> next x);
        kfragment       = (fun (next, self) f  -> next f);
        kformat         = (fun (next, self) f  -> next f);
        kinfo           = (fun (next, self) ii  -> next ii);
        knode           = (fun (next, self) n  -> next node);
        ktoplevel       = (fun (next, self) p  -> next p);
        kcppdirective   = (fun (next, self) p  -> next p);
        kifdefdirective = (fun (next, self) p  -> next p);
        kdefineval      = (fun (next, self) p  -> next p);
        kstatementseq   = (fun (next, self) p  -> next p);
        kfield          = (fun (next, self) p  -> next p);
    } in
    Visitor_c.vk_program vistor ast