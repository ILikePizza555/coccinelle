open Common

let graph_cocci (file : Common.filename) = ()

let graph_c (file : Common.filename) = 
    if not (file =~ ".*\\c")
    then pr2 "warning: seems not a c file";

    let (xs, stat) = Parse_c.parse_c_and_cpp true false file in
    xs +> List.iter (fun (ast, (s, toks)) -> 

    )