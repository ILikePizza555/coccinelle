open Common

let graph_cocci (file : Common.filename) = ()

let graph_c (file : Common.filename) = 
    if not (file =~ ".*\\c")
    then pr2 "warning: seems not a c file";

    let (tl2, _) = Parse_c.parse_c_and_cpp true false file in
    let prog = Parse_c.program_of_program2 tl2 in
    let graph = Dot.graph_from_c_ast prog in
    pr2 (Dot.string_of_graph graph)