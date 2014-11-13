(* Muhammad Ali Akbar *)

open Icode
open Vars

let gencpp_init =
    "#include <iostream>\n#include \"lame.h\"\n\nusing namespace std;\n\nint main(void)\n{\n"

let gencpp_end =
    "\treturn 0;\n}\n"

let string_of_bool bv = 
    match bv with
        true   -> "TRUE"
      | false  -> "FALSE"

let gencpp_scalval sv vt = 
    match sv with
        SLit n -> string_of_float n
      | SVar n -> get_variable_name n vt

let gencpp_boolval bv vt = 
    match bv with
        BLit n -> string_of_bool n
      | BVar n -> get_variable_name n vt

let gencpp i vt =
    match i with
        SCAL (lhs, rhs)        -> let name = get_variable_name lhs vt
                                  and cppexp = gencpp_scalval rhs vt
                                  in
                                      name ^ " = " ^ cppexp ^ ";\n"
      | BOOL (lhs, rhs)        -> let name = get_variable_name lhs vt
                                  and cppexp = gencpp_boolval rhs vt
                                  in
                                      name ^ " = " ^ cppexp ^ ";\n"
      | STR (lhs, rhs)         -> let name = get_variable_name lhs vt
                                  and cppexp = rhs
                                  in
                                      name ^ " = " ^ cppexp ^ ";\n"
      | ADD (lhs, rhs1, rhs2)  -> let name = get_variable_name lhs vt
                                  and cppexp1 = gencpp_scalval rhs1 vt
                                  and cppexp2 = gencpp_scalval rhs2 vt
                                  in
                                      name ^ " = " ^ cppexp1 ^ " + " ^ cppexp2 ^ ";\n"
      | SUB (lhs, rhs1, rhs2)  -> let name = get_variable_name lhs vt
                                  and cppexp1 = gencpp_scalval rhs1 vt
                                  and cppexp2 = gencpp_scalval rhs2 vt
                                  in
                                      name ^ " = " ^ cppexp1 ^ " - " ^ cppexp2 ^ ";\n"
      | MUL (lhs, rhs1, rhs2)  -> let name = get_variable_name lhs vt
                                  and cppexp1 = gencpp_scalval rhs1 vt
                                  and cppexp2 = gencpp_scalval rhs2 vt
                                  in
                                      name ^ " = " ^ cppexp1 ^ " * " ^ cppexp2 ^ ";\n"
      | DIV (lhs, rhs1, rhs2)  -> let name = get_variable_name lhs vt
                                  and cppexp1 = gencpp_scalval rhs1 vt
                                  and cppexp2 = gencpp_scalval rhs2 vt
                                  in
                                      name ^ " = " ^ cppexp1 ^ " / " ^ cppexp2 ^ ";\n"
      | POW (lhs, rhs1, rhs2)  -> let name = get_variable_name lhs vt
                                  and cppexp1 = gencpp_scalval rhs1 vt
                                  and cppexp2 = gencpp_scalval rhs2 vt
                                  in
                                      name ^ " = pow(" ^ cppexp1 ^ ", " ^ cppexp2 ^ ");\n"
      | MADD (lhs, rhs1, rhs2) -> let name = get_variable_name lhs vt
                                  and cppexp1 = get_variable_name rhs1 vt
                                  and cppexp2 = get_variable_name rhs2 vt
                                  in
                                      name ^ " = " ^ cppexp1 ^ " + " ^ cppexp2 ^ ";\n"
      | MSUB (lhs, rhs1, rhs2) -> let name = get_variable_name lhs vt
                                  and cppexp1 = get_variable_name rhs1 vt
                                  and cppexp2 = get_variable_name rhs2 vt
                                  in
                                      name ^ " = " ^ cppexp1 ^ " - " ^ cppexp2 ^ ";\n"
      | MMUL (lhs, rhs1, rhs2) -> let name = get_variable_name lhs vt
                                  and cppexp1 = get_variable_name rhs1 vt
                                  and cppexp2 = get_variable_name rhs2 vt
                                  in
                                      name ^ " = " ^ cppexp1 ^ " * " ^ cppexp2 ^ ";\n"
      | SMUL (lhs, rhs1, rhs2) -> let name = get_variable_name lhs vt
                                  and cppexp1 = get_variable_name rhs1 vt
                                  and cppexp2 = gencpp_scalval rhs2 vt
                                  in
                                      "LAMEScalarMul(" ^ name ^ ", " ^ cppexp1 ^ ", " ^ cppexp2 ^ ");\n"
      | MPOW (lhs, rhs1, rhs2) -> let name = get_variable_name lhs vt
                                  and cppexp1 = get_variable_name rhs1 vt
                                  and cppexp2 = gencpp_scalval rhs2 vt
                                  in
                                      "LAMEMatPow(" ^ name ^ ", " ^ cppexp1 ^ ", " ^ cppexp2 ^ ");\n"
      | INIT (_)               ->       "\n"
      | SET  (lhs, rhs1, rhs2, rhs3) -> let name = get_variable_name lhs vt
                                  and cppexp1 = gencpp_scalval rhs1 vt
                                  and cppexp2 = gencpp_scalval rhs2 vt
                                  and cppexp3 = gencpp_scalval rhs3 vt
                                  in
                                      "LAMESetElem(" ^ name ^ ", " ^ cppexp1 ^ ", " ^ cppexp2 ^ ", " ^ cppexp3 ^ ");\n"
      | GET  (lhs, rhs1, rhs2, rhs3) -> let name = get_variable_name lhs vt
                                  and cppexp1 = gencpp_scalval rhs1 vt
                                  and cppexp2 = gencpp_scalval rhs2 vt
                                  and cppexp3 = get_variable_name rhs3 vt
                                  in
                                      "LAMEGetElem(" ^ cppexp3 ^ ", " ^ name ^ ", " ^ cppexp1 ^ ", " ^ cppexp2 ^ ");\n"
      | TRAN (lhs, rhs)        -> let name = get_variable_name lhs vt
                                  and cppexp = get_variable_name rhs vt
                                  in
                                      "LAMEMatTrans(" ^ name ^ ", " ^ cppexp ^ ");\n"
      | DIM (lhs, rhs1, rhs2)  -> let name = get_variable_name lhs vt
                                  and cppexp1 = gencpp_scalval rhs1 vt
                                  and cppexp2 = gencpp_scalval rhs2 vt
                                  in
                                      "LAMEMatDim(" ^ name ^ ", " ^ cppexp1 ^ ", " ^ cppexp2 ^ ");\n"
      | PRINT (lhs)            -> let name = get_variable_name lhs vt
                                  in
                                      "LAMEPrint(" ^ name ^ ");\n"
      | SINI (_)               ->     "\n"
      | SCPY (lhs, rhs)        -> let name = get_variable_name lhs vt
                                  and cppexp = get_variable_name rhs vt
                                  in
                                      name ^ " = " ^ cppexp ^ ";\n"
      | SFRE (_)               ->     "\n"
      | SCAT (lhs, rhs1, rhs2) -> let name = get_variable_name lhs vt
                                  and cppexp1 = get_variable_name rhs1 vt
                                  and cppexp2 = get_variable_name rhs2 vt
                                  in
                                      "LAMEStrConcat(" ^ name ^ ", " ^ cppexp1 ^ ", " ^ cppexp2 ^ ");\n"
      | SCMP (lhs, rhs1, rhs2) -> let name = get_variable_name lhs vt
                                  and cppexp1 = get_variable_name rhs1 vt
                                  and cppexp2 = get_variable_name rhs2 vt
                                  in
                                      "LAMEStrCmp(" ^ name ^ ", " ^ cppexp1 ^ ", " ^ cppexp2 ^ ");\n"
      | SFSC (lhs, rhs)        -> let name = get_variable_name lhs vt
                                  and cppexp = gencpp_scalval rhs vt
                                  in
                                      "LAMEStrFromScalar(" ^ name ^ ", " ^ cppexp ^ ");\n"
      | SFMA (lhs, rhs)        -> let name = get_variable_name lhs vt
                                  and cppexp = get_variable_name rhs vt
                                  in
                                      "LAMEStrFromMat(" ^ name ^ ", " ^ cppexp ^ ");\n"
      | SLT (lhs, rhs1, rhs2)  -> let name = get_variable_name lhs vt
                                  and cppexp1 = gencpp_scalval rhs1 vt
                                  and cppexp2 = gencpp_scalval rhs2 vt
                                  in
                                      name ^ " = (" ^ cppexp1 ^ " < " ^ cppexp2 ^ ");\n"
      | SLE (lhs, rhs1, rhs2)  -> let name = get_variable_name lhs vt
                                  and cppexp1 = gencpp_scalval rhs1 vt
                                  and cppexp2 = gencpp_scalval rhs2 vt
                                  in
                                      name ^ " = (" ^ cppexp1 ^ " <= " ^ cppexp2 ^ ");\n"
      | SGT (lhs, rhs1, rhs2)  -> let name = get_variable_name lhs vt
                                  and cppexp1 = gencpp_scalval rhs1 vt
                                  and cppexp2 = gencpp_scalval rhs2 vt
                                  in
                                      name ^ " = (" ^ cppexp1 ^ " > " ^ cppexp2 ^ ");\n"
      | SGE (lhs, rhs1, rhs2)  -> let name = get_variable_name lhs vt
                                  and cppexp1 = gencpp_scalval rhs1 vt
                                  and cppexp2 = gencpp_scalval rhs2 vt
                                  in
                                      name ^ " = (" ^ cppexp1 ^ " >= " ^ cppexp2 ^ ");\n"
      | SEQ (lhs, rhs1, rhs2)  -> let name = get_variable_name lhs vt
                                  and cppexp1 = gencpp_scalval rhs1 vt
                                  and cppexp2 = gencpp_scalval rhs2 vt
                                  in
                                      name ^ " = (" ^ cppexp1 ^ " == " ^ cppexp2 ^ ");\n"
      | SNE (lhs, rhs1, rhs2)  -> let name = get_variable_name lhs vt
                                  and cppexp1 = gencpp_scalval rhs1 vt
                                  and cppexp2 = gencpp_scalval rhs2 vt
                                  in
                                      name ^ " = (" ^ cppexp1 ^ " != " ^ cppexp2 ^ ");\n"
      | SMEQ (lhs, rhs1, rhs2) -> let name = get_variable_name lhs vt
                                  and cppexp1 = get_variable_name rhs1 vt
                                  and cppexp2 = get_variable_name rhs2 vt
                                  in
                                      name ^ " = (" ^ cppexp1 ^ " == " ^ cppexp2 ^ ");\n"
      | SMNE (lhs, rhs1, rhs2) -> let name = get_variable_name lhs vt
                                  and cppexp1 = get_variable_name rhs1 vt
                                  and cppexp2 = get_variable_name rhs2 vt
                                  in
                                      name ^ " = (" ^ cppexp1 ^ " != " ^ cppexp2 ^ ");\n"

      | OR (lhs, rhs1, rhs2)   -> let name = get_variable_name lhs vt
                                  and cppexp1 = gencpp_boolval rhs1 vt
                                  and cppexp2 = gencpp_boolval rhs2 vt
                                  in
                                      name ^ " = (" ^ cppexp1 ^ " || " ^ cppexp2 ^ ");\n"
      | AND (lhs, rhs1, rhs2)   -> let name = get_variable_name lhs vt
                                  and cppexp1 = gencpp_boolval rhs1 vt
                                  and cppexp2 = gencpp_boolval rhs2 vt
                                  in
                                      name ^ " = (" ^ cppexp1 ^ " && " ^ cppexp2 ^ ");\n"
      | NOT (lhs, rhs)         -> let name = get_variable_name lhs vt
                                  and cppexp = gencpp_boolval rhs vt
                                  in
                                      name ^ " = !(" ^ cppexp ^ ");\n"
      | BRAF (lhs, rhs)        -> let cppexp = gencpp_boolval lhs vt
                                  and name = get_variable_name rhs vt
                                  in
                                      "if(!(" ^ cppexp ^ ")) goto " ^ name ^ ";\n"
      | JMP (lhs)              -> let name = get_variable_name lhs vt
                                  in
                                      "goto " ^ name ^ ";\n"
      | LABL (lhs)             -> let name = get_variable_name lhs vt
                                  in
                                      name ^ ":\n"
      | ROWS (lhs, rhs)        -> let name = get_variable_name lhs vt
                                  and cppexp = get_variable_name rhs vt
                                  in
                                      "LAMEMatRows(" ^ name ^ ", " ^ cppexp ^ ");\n"
      | COLS (lhs, rhs)        -> let name = get_variable_name lhs vt
                                  and cppexp = get_variable_name rhs vt
                                  in
                                      "LAMEMatCols(" ^ name ^ ", " ^ cppexp ^ ");\n"
      | RDIM (lhs, rows, cols) -> let name = get_variable_name lhs vt
                                  and cppexp1 = gencpp_scalval rows vt
                                  and cppexp2 = gencpp_scalval cols vt
                                  in
                                      "LAMEMatDim(" ^ name ^", " ^ cppexp1 ^ ", " ^ cppexp2 ^ ");\n"

let rec gencpp_prog_rec iprog vt =
    match iprog with
        i :: is -> "\t" ^ (gencpp i vt) ^ (gencpp_prog_rec is vt)
      | []      -> ""

let gencpp_prog iprog vt =
    gencpp_init ^ (gencpp_vartable vt) ^ (gencpp_prog_rec iprog vt) ^ gencpp_end
