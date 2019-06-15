open Common

let graph_cocci (file : Common.filename) = ()

(* A special `vistor_c` that maps the C AST to a DOT graph *)
let c_dot_map = {
    kexpr           = (fun (k, _) expr -> pr2 expr; k expr);
    kassignOp       = (fun (k,_) op -> k op);
    kbinaryOp       = (fun (k,_) op -> k op);
    kstatement      = (fun (k,_) st -> k st);
    ktype           = (fun (k,_) t  -> k t);
    kdecl           = (fun (k,_) d  -> k d);
    konedecl        = (fun (k,_) d  -> k d);
    konedecl_opt    = (fun _ (k,_) d  -> k d);
    kparam          = (fun (k,_) d  -> k d);
    kdef            = (fun (k,_) d  -> k d);
    kini            = (fun (k,_) ie  -> k ie);
    kname           = (fun (k,_) x -> k x);
    kfragment       = (fun (k,_) f  -> k f);
    kformat         = (fun (k,_) f  -> k f);
    kinfo           = (fun (k,_) ii  -> k ii);
    knode           = (fun (k,_) n  -> k n);
    ktoplevel       = (fun (k,_) p  -> k p);
    kcppdirective   = (fun (k,_) p  -> k p);
    kifdefdirective = (fun (k,_) p  -> k p);
    kdefineval      = (fun (k,_) p  -> k p);
    kstatementseq   = (fun (k,_) p  -> k p);
    kfield          = (fun (k,_) p  -> k p);
}

let graph_c (file : Common.filename) = 
    if not (file =~ ".*\\c")
    then pr2 "warning: seems not a c file";

    let (xs, stat) = Parse_c.parse_c_and_cpp true false file in
    xs +> List.iter (fun (ast, (s, toks)) -> 

    )