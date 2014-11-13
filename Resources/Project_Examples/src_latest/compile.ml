open Ast

module StringMap = Map.Make(String)

(* Symbol table: Information about all the names in scope *)
type env = {
    function_type : dataType StringMap.t; (* <name, return_type>   *)
    global_var_type  : dataType StringMap.t; (* <global_var,type> *)
    local_var_type   : dataType  StringMap.t; (* <local_var,type> *)
  }


let string_of_vdecl var = string_of_dataType var.var_type ^ " " ^ var.var_name ^ ";\n"

let string_of_gvdecl var = "private static " ^ string_of_dataType var.var_type ^ " " ^ var.var_name ^ ";\n"

(* 
Make a tuple
Input: list of declarations
Ouput: list of tuples: (name,type)
*)
(* val enum : int -> 'a list -> (int * 'a) list *)
let rec enum_var  = function
    [] -> []
  | hd::tl -> (hd.var_type, hd.var_name) :: enum_var tl

let rec enum_func = function
    [] -> []
 |  hd:: tl -> (hd.func_type, hd.func_name) :: enum_func tl

(* Add every tuples in the string map *)
(* val string_map_pairs StringMap 'a -> (Datatype * 'a) list -> StringMap 'a *)
let string_map_pairs map pairs =
  List.fold_left (fun m (i, n) -> StringMap.add n i m) map pairs


(** Translate a program in AST form into a Java program while doing type checking at the same time.
    Raise a Failure if something is wrong.  **)
(* program = entry_point * dec list *)
let translate (vars_decs, funs_decs) = 
 (* record global variables *)
 let global_variables = string_map_pairs StringMap.empty (enum_var vars_decs) in 

  (* add built-in support for "print"  *)
  let built_in_functions = StringMap.add "print" Int StringMap.empty in
  let function_types = string_map_pairs built_in_functions
      (enum_func funs_decs) in

  (* Translate a function in AST form into Java statements *)
  let translate_function env fdecl =
    let num_formals = List.length fdecl.formals
    and num_locals = List.length fdecl.locals
    and local_var_type_tuples = enum_var fdecl.locals
    and formal_var_type_tuples = enum_var fdecl.formals in
    let env = { env with local_var_type = string_map_pairs
		  StringMap.empty (local_var_type_tuples @ formal_var_type_tuples) } in

    let rec getType =function
      Id s -> (try StringMap.find s env.local_var_type
              with Not_found -> try StringMap.find s env.global_var_type
              with Not_found -> raise (Failure ("undeclared variable")) )
      | Int_lit i-> Int  
      | Float_lit i-> Float 
      | Det (e) -> (match (getType e) with
                      IntMatrix -> Int
                      | FloatMatrix -> Float
                      | _  -> raise (Failure("Illegal Det Operation")) )
      | Trace e -> (match (getType e) with
                      IntMatrix -> Int
                      | FloatMatrix -> Float
                      | _  -> raise (Failure("Illegal Trace Operation")) )
      | SubMat (e1, e2, e3, e4, e5) -> (match (getType e1) with
                                        IntMatrix -> IntMatrix
                                        | FloatMatrix -> FloatMatrix 
                                        | _ -> raise (Failure("Illegal SubMat Operation")))
      | AreaSum (e1, e2, e3, e4, e5) -> (match (getType e1) with 
                                  IntMatrix -> Int
                                  | FloatMatrix -> Float
                                  | _ -> raise (Failure("Illegal AreaSum Operation")) )
      | Binary_op (e1, bop, e2) -> let e1Type=getType e1 and e2Type =getType e2
                                  in if (bop == Add || bop == Sub || bop == Times)
                                  then if( (e1Type == IntMatrix || e1Type == FloatMatrix) && (e2Type == IntMatrix || e2Type == FloatMatrix))
                                            then if (e1Type == IntMatrix && e2Type == IntMatrix) 
                                                  then IntMatrix
                                                  else FloatMatrix 
                                            else if((e1Type == IntMatrix || e1Type == FloatMatrix))
                                                  then FloatMatrix
                                                  else if (e2Type == IntMatrix || e2Type == FloatMatrix)
                                                       then FloatMatrix
                                                       else if (e1Type == Int && e2Type == Int)
                                                            then Int
                                                            else Float
                                  else if (bop == Mode)
                                          then if(e1Type == Int && e2Type == Int)
                                                then Int
                                                else raise (Failure "Illegal Mod Operation")
                                          else if (bop == Divides)
                                               then if ( (e1Type == IntMatrix || e1Type == FloatMatrix) && (e2Type == IntMatrix || e2Type == FloatMatrix))
                                                    then raise (Failure "Illegal Divides Operation")
                                                    else if ((e1Type == Float && e2Type == Int) || (e1Type == Float && e2Type == Float) || (e1Type == Int && e2Type == Float) || (e1Type == Int && e2Type == Int))
                                                         then Float
                                                         else if ( e1Type == Int && e2Type == Int)
                                                              then  Int
                                                              else if ((e2Type == Int) || (e2Type == Float)) (* when e1 is a matrix*)
                                                                    then FloatMatrix
                                                                    else raise (Failure ("Illegal Divides Operation: Trying to divide a number by a matrix"))
                                               else if( (e1Type == IntMatrix || e1Type == FloatMatrix) && (e2Type == IntMatrix || e2Type == FloatMatrix)) (*when bop = Convolution *)
                                              then if( e1Type == FloatMatrix || e2Type == FloatMatrix )
                                                  then FloatMatrix
                                                  else IntMatrix
                                              else raise (Failure ("Illegal Convolution Operation"))
      | Unary_op (e, uop) -> let eType=getType e 
                            in (match uop with
                                Inverse -> FloatMatrix
                                | Transpo -> if(eType == FloatMatrix)
                                            then FloatMatrix
                                            else if (eType == IntMatrix)
                                                  then IntMatrix
                                                  else raise (Failure ("Illegal Transpose Operation"))
                                | Neg -> if(eType == Int || eType == Float)
                                      then eType
                                      else raise (Failure ("Illegal Negate Operation: can only be applied to Int and Float"))
                                | _ -> raise (Failure ("Not Supported Yet "))
                                    ) 
      | Part_of_mat (e1, e2, e3) -> let e1Type= getType e1 and e2Type = getType e2 and e3Type= getType e3
                                    in if ((e1Type == IntMatrix || e1Type == FloatMatrix) && (e2Type == Int) && (e3Type == Int))
                                       then if (e1Type == IntMatrix)
                                            then Int
                                            else Float
                                       else raise (Failure ("Illegal Matrix Element Access"))
      | Boolean s -> Int 
      | Im_init ( lle ) -> FloatMatrix
      | Float_init (lle ) -> FloatMatrix
      | Call (s, le) -> (try StringMap.find s env.function_type 
                        with Not_found -> raise (Failure ("undeclared function")) )
      | VarAssign (s, e) -> (try StringMap.find s env.local_var_type 
                            with Not_found -> try StringMap.find s env.global_var_type
                            with Not_found -> raise (Failure ("undeclared variable")) )
      | ElemAssign (s, e1, e2, e3) -> getType e3                         
      | _ -> raise (Failure ("Not supported yet"))
      
      in let rec checkParas formals = function 
        [] -> true 
      | hd::tl -> if( (getType hd) == (List.hd formals).var_type )
                  then checkParas (List.tl formals) tl
                  else if ( (List.hd formals).var_type == Float && (getType hd) == Int )
                       then checkParas (List.tl formals) tl
                       else false 

      in let rec expr = function
        Det e -> let eType= getType e in 
                if (eType == IntMatrix || eType == FloatMatrix)
                then  "Matrix.matrixDet(" ^ expr e ^ ")"
                else raise (Failure ("Illegal Det Operation: trying to Det a non-matrix"))
      | Trace e -> let eType = getType e in 
                if (eType == IntMatrix || eType == FloatMatrix)
                then  "Matrix.matrixTrace(" ^ expr e ^ ")"
                else raise (Failure ("Illegal Trace Operation: trying to Trace a non-matrix"))
      | SubMat (e1, e2, e3, e4, e5) -> if( (getType e1 == IntMatrix || getType e1 == FloatMatrix) && getType e2 == Int && getType e3 == Int && getType e4 == Int && getType e5 == Int)
                                      then "Matrix.subMat(" ^ expr e1 ^ ", " ^ expr e2 ^ ", " ^ expr e3 ^ ", " ^ expr e4 ^ ", " ^ expr e5 ^ ")"
                                      else raise (Failure ("Illegal SubMat operation"))
      | AreaSum (e1, e2, e3, e4, e5) -> if( (getType e1 == IntMatrix || getType e1 == FloatMatrix) && getType e2 == Int && getType e3 == Int && getType e4 == Int && getType e5 == Int)
                                      then "Matrix.areaSum(" ^ expr e1 ^ ", " ^ expr e2 ^ ", " ^ expr e3 ^ ", " ^ expr e4 ^ ", " ^ expr e5 ^ ")"
                                      else raise (Failure ("Illegal AreaSum operation"))
      | Binary_op (e1, bop, e2) -> let e1Type=getType e1 and e2Type =getType e2
                                  in if (bop == Add || bop == Sub || bop == Times)
                                  then if( (e1Type == IntMatrix || e1Type == FloatMatrix) && (e2Type == IntMatrix || e2Type == FloatMatrix))
                                            then (match bop with
                                                  Add -> "Matrix.matrixPlus(" ^ expr e1 ^ "," ^ expr e2 ^ ")"
                                                  | Sub -> "Matrix.matrixMinus(" ^ expr e1 ^ "," ^ expr e2 ^ ")"
                                                  | Times -> "Matrix.matrixTimes(" ^ expr e1 ^ "," ^ expr e2 ^ ")" 
                                                  | _ -> "/* Impossible */")
                                            else if((e1Type == IntMatrix || e1Type == FloatMatrix))
                                                  then (match bop with
                                                  Add -> "Matrix.matrixPlusDigit(" ^ expr e1 ^ "," ^ expr e2 ^ ")"
                                                  | Sub -> "Matrix.matrixMinusDigit(" ^ expr e1 ^ "," ^ expr e2 ^ ")"
                                                  | Times -> "Matrix.matrixScalarT(" ^ expr e1 ^ "," ^ expr e2 ^ ")" 
                                                  | _ -> "/* Impossible */")
                                                  else if (e2Type == IntMatrix || e2Type == FloatMatrix)
                                                       then (match bop with
                                                            Add -> "Matrix.matrixPlusDigit(" ^ expr e1 ^ "," ^ expr e2 ^ ")"
                                                            | Sub -> "Matrix.matrixMinusDigit(" ^ expr e1 ^ "," ^ expr e2 ^ ")"
                                                            | Times -> "Matrix.matrixScalarT(" ^ expr e1 ^ "," ^ expr e2 ^ ")" 
                                                            | _ -> "/* Impossible */")
                                                       else "(" ^ expr e1 ^ string_of_bin_op bop ^ expr e2 ^")"
                                  else if (bop == Mode)
                                          then if(e1Type == Int && e2Type == Int)
                                                then expr e1 ^ "%" ^ expr e2 
                                                else raise (Failure "Illegal Mod Operation")
                                          else if (bop == Divides)
                                               then if ( (e1Type == IntMatrix || e1Type == FloatMatrix) && (e2Type == IntMatrix || e2Type == FloatMatrix))
                                                    then raise (Failure "Illegal Divides Operation")
                                                    else if ((e1Type== Int || e1Type == Float) && (e2Type == Int || e2Type == Float)) 
                                                         then expr e1 ^ "/" ^ expr e2
                                                         else if ((e2Type == Int) || (e2Type == Float)) (* when e1 is a matrix*)
                                                              then "Matrix.matrixScalarD(" ^ expr e1 ^ "," ^ expr e2 ^ ")"
                                                              else raise (Failure ("Illegal Divides Operation: Trying to divide a number by a matrix"))
                                               else if( (e1Type == IntMatrix || e1Type == FloatMatrix) && (e2Type == IntMatrix || e2Type == FloatMatrix)) (*when bop = Convolution *)
                                                    then "Matrix.matrixConv(" ^ expr e1 ^ "," ^ expr e2 ^ ")"
                                                    else raise (Failure ("Illegal Convolution Operation"))
      | Unary_op (e, uop) -> let eType = getType e 
                            in (match uop with
                                Inverse -> if ( eType == IntMatrix || eType == FloatMatrix)
                                           then "Matrix.matrixInverse(" ^ expr e ^ ")"
                                           else raise (Failure ("Illegal Inverse Operation"))
                                | Transpo -> if ( eType == IntMatrix || eType == FloatMatrix)
                                            then "Matrix.matrixTrans(" ^ expr e ^ ")"
                                            else raise (Failure ("Illegal Transpose Operation"))
                                | Neg -> if(eType == Int || eType == Float)
                                      then "-" ^ expr e
                                      else raise (Failure ("Illegal Negate Operation: can only be applied to Int and Float"))
                                | _ -> raise (Failure ("Not Supported Yet "))
                                    ) 
      | Id s -> if (StringMap.mem s env.local_var_type) then s 
                else if (StringMap.mem s env.global_var_type)
                      then s
                      else raise (Failure ("undeclared variable " ^ s))
      | Float_lit i -> string_of_float i
      | Int_lit i ->  string_of_int i

      | Part_of_mat (e1, e2, e3) -> let e1Type= getType e1 and e2Type = getType e2 and e3Type= getType e3
                                    in if ((e1Type == IntMatrix || e1Type == FloatMatrix) && (e2Type == Int) && (e3Type == Int))
                                       then expr e1 ^ "[" ^ expr e2 ^ "]" ^ "[" ^ expr e3 ^ "]" 
                                       else raise (Failure ("Illegal Matrix Element Access"))
      | Boolean s -> if ((String.compare s "true") == 0)
                      then "1" 
                      else "0"
      | Im_init lle -> "Matrix.floatMatrixInit(\"" ^  (String.concat ";" (List.map (fun le -> "" ^ (String.concat "," (List.map (fun e -> if ((getType e) != Int && (getType e) != Float) 
                                                                      then raise (Failure ("Illegal Initialization of an Float Matrix"))
                                                                      else expr e) le ))  ^ "" ) lle)) ^ "\")" 
      | Float_init lle -> "/* Not Supported Yet */"
      | Call (s, le) -> if((String.compare s "print") == 0)
                        then (if ( (List.length le) != 1)
                                  then raise (Failure ("Illegal Call To Print: Wrong number of arguments"))
                                  else 
                                  	let eType = getType (List.hd le) in 
                                  	if(eType == FloatMatrix) then "Matrix.show("^ expr (List.hd le) ^ ")" 
                                  	 else  "System.out.println(" ^ expr (List.hd le) ^ ")" )
                        else if(StringMap.mem s env.function_type)
                             then  String.concat "" (List.map (fun fdecl -> if( (String.compare fdecl.func_name s) == 0 ) 
                                                     then if ((List.length fdecl.formals) == (List.length le))
                                                          then if (checkParas fdecl.formals le)
                                                               then s ^ "(" ^ (String.concat ", " (List.map expr le)) ^ ")"
                                                               else raise (Failure ("Incompatible parameter types"))
                                                          else raise (Failure ("Illegal function call to " ^ s ^ ": Wrong number of parameters"))
                                                     else "" ) funs_decs)
                             else raise (Failure ("undeclared function"))
      | VarAssign (s, e) -> (try (if((StringMap.find s env.local_var_type) == getType e || ((StringMap.find s env.local_var_type) == Float && getType e == Int))  then s ^ " = " ^ expr e else raise (Failure ("Illegal variable Assignment: Incompatible Type： " ^ string_of_dataType (getType e))))
                            with Not_found ->( try (if ((StringMap.find s env.global_var_type) == getType e || ((StringMap.find s env.local_var_type) == Float && getType e == Int)) then s ^ " = " ^ expr e else raise (Failure ("Illegal variable Assignment: Incompatible Type： " ^ string_of_dataType (getType e))))
                                              with Not_found -> raise (Failure ("undeclared variable"))))
      | ElemAssign (s, e1, e2, e3) -> if(getType e1 == Int && getType e2 = Int)
                                      then 
                                      (try (if(((StringMap.find s env.local_var_type) == IntMatrix && getType e3 == Int ) || ((StringMap.find s env.local_var_type) == FloatMatrix && (getType e3 == Int || getType e3 == Float)))  then s ^ "[" ^ expr e1 ^ "][" ^expr e2 ^ "] = " ^ expr e3  else raise (Failure ("Illegal Element Assignment: Incompatible Type： " ^ string_of_dataType (getType e3))))
                                                with Not_found ->( try (if (((StringMap.find s env.global_var_type) == IntMatrix && getType e3 == Int ) || ((StringMap.find s env.global_var_type) == FloatMatrix && (getType e3 == Int || getType e3 == Float))) then s ^ "[" ^ expr e1 ^ "][" ^expr e2 ^ "] = " ^ expr e3  else raise (Failure ("Illegal Element Assignment: Incompatible Type： " ^ string_of_dataType (getType e3))))
                                                                  with Not_found -> raise (Failure ("undeclared variable"))))
                                    else raise (Failure ("Illegal Element Assignment: Index not integer"))
      | _ -> "Not supported yet"
                    
      in let rec b_expr = function 
        Bool_expr1 (e1, boop1, e2) -> let e1Type = getType e1 and e2Type = getType e2
                                    in if((e1Type == Int || e1Type == Float) && (e2Type == Int || e2Type == Float ))
                                      then expr e1 ^ string_of_bool_op1 boop1 ^ expr e2
                                      else raise (Failure ("Illegal Boolean Expression: " ^ expr e1 ^ string_of_bool_op1 boop1 ^ expr e2))
      | Bool_expr2 (b1, boop2, b2) -> if(boop2 == Not)
                                      then "!(" ^ b_expr b1 ^ ")"
                                      else b_expr b1 ^ string_of_bool_op2 boop2 ^ b_expr b2


    in let rec stmt = function
	     Block sl     ->  "{\n" ^ String.concat "" (List.map stmt (List.rev sl)) ^ "\n}\n"
      | Expr e       -> expr e ^ ";\n"
      | Ifelse (e, s1, s2) -> "if (" ^ b_expr e ^")\n{" ^ stmt s1 ^ "}\nelse\n{" ^ stmt s2 ^"}" 
      | If (e, s) -> "if (" ^ b_expr  e ^")\n" ^ "{" ^ stmt s ^"}"
      | For (e1, e2, e3, s) -> "for (" ^ expr e1 ^ "; " ^ b_expr e2 ^ "; " ^ expr e3 ^
        ")\n" ^ stmt s
      | While (e, s) -> "while (" ^ b_expr e ^ ")\n" ^ stmt s  
      | Export s -> "/* Export not supported */"
      | Init (s, e1, e2) -> (s ^ " = new " ^ try string_of_matrixType_Init (StringMap.find s env.local_var_type)
                            with Not_found -> try string_of_matrixType_Init (StringMap.find s env.global_var_type )
                            with Not_found -> raise (Failure ("undeclared variable" ^ s))) 
                            ^ "[" ^ expr e1 ^ "][" ^ expr e2 ^ "];\n" 
      | Import (s1, s2) -> "/* Import not supported */"
      | Return e -> "return " ^ expr e ^ ";\n"
      | Empty -> ""

    in let name = fdecl.func_name in (if ((String.compare name "entry") == 0) 
                                    then "public static void main(String[] args) throws Exception\n" ^ (if ( (List.length fdecl.formals) != 0) then raise (Failure ("Illegal Entry declaration: entry function must not have any arguments!")) else "")
                                  else "public static " ^ string_of_dataType fdecl.func_type ^ " " ^ fdecl.func_name
      ^ if ((List.length fdecl.formals) != 0)
        then ("("
      ^ string_of_dataType (List.hd fdecl.formals).var_type ^ " " ^ (List.hd fdecl.formals).var_name ^ " "
      ^ String.concat "" (List.map (fun formal -> ", " ^ string_of_dataType formal.var_type ^ " " ^ formal.var_name) (List.tl fdecl.formals)) ^ ") throws Exception\n")
        else "() throws Exception\n"
        )
      ^ "{\n" ^String.concat "" (List.map string_of_vdecl (List.rev fdecl.locals)) ^ "\n"
      ^ stmt (Block fdecl.body) ^ "}\n" 
    
    in let env = { function_type = function_types;
  		 global_var_type = global_variables;
  		 local_var_type = StringMap.empty } in

    (* program starts here: *)
    let entry_function = try
        (StringMap.find "entry" function_types)
    with Not_found -> raise (Failure ("no \"entry\" function"))
    
    in "public class MatCab {\n" ^
      String.concat "" (List.map string_of_gvdecl (List.rev vars_decs)) ^ "\n" ^
        (String.concat "\n" (List.map (translate_function env) (List.rev funs_decs))) ^ "\n}"


