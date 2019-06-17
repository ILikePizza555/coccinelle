open Common
open Ast_c

let name_from_ii = List.fold_left (fun acc curr -> acc ^ (
        match curr.pinfo with
        | OriginTok pi -> 
            Printf.sprintf "_%i_%i_%s_" pi.line pi.charpos pi.str
        | FakeTok (s, (pi, off)) ->
            Printf.sprintf "f_%i_%i_%s_" pi.line (pi.charpos + off) s
        | ExpandedTok (pi, _) ->
            Printf.sprintf "e_%i_%i_%s_" pi.line pi.charpos pi.str
        | AbstractLineTok pi ->
            Printf.sprintf "a_%i_%i_%s_" pi.line pi.charpos pi.str)) ""

(* Placeholder names for unsupported/unknown nodes *)
let name_from_unknown (ii : Ast_c.il) = "unk_" ^ (name_from_ii ii)

(* Most of the name_from_ast functions simply call name_from_ii. The point
of these functions to make generating the names of the child nodes easier and
more consistent. *)
let name_from_expr ((_, ii) : Ast_c.expression) = "expr_" ^ (name_from_ii ii)
let name_from_assign_op ((_, ii) : Ast_c.assignOp) = "ao_" ^ (name_from_ii ii)
let name_from_binary_op ((_, ii) : Ast_c.binaryOp) = "bo_" ^ (name_from_ii ii)
let name_from_statement ((_, ii) : Ast_c.statement) = "stat_" ^ (name_from_ii ii)