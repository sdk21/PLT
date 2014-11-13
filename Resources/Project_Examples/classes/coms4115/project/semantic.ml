(* David Golub *)

open Ast
open Icode
open Labels
open Vars

exception InvalidMatrix
exception TypeMismatch
exception WrongDataType

let rec check_cols row temp i j =
    match row with
        hd :: tl -> let sets = check_cols tl temp i (j + 1)
                    in
                        SET (temp, SLit (float_of_int i),
                             SLit (float_of_int j), SLit hd) :: sets
      | []       -> []

let rec check_rows m temp i =
    match m with
        hd :: tl -> let cols1 = List.length hd
                    and (cols2, sets2) = check_rows tl temp (i + 1)
                    in
                        if cols1 = cols2 || cols2 = 0 then
                            let sets1 = check_cols hd temp i 0
                            in
                                (cols1, (sets1 @ sets2))
                        else
                            raise InvalidMatrix
      | []       -> (0, [])

let check_matrix m vt = 
    let temp = add_temp Matrix vt
    in
        let (_, sets) = check_rows m temp 0
        in
            let instrs = [INIT temp] @ sets
            in (temp, instrs)

let rec check_expr e vt =
    match e with
        NumLit n        -> let temp = add_temp Scalar vt
                           in
                               let instrs = [SCAL (temp, SLit n)]
                               in (Scalar, temp, instrs)
      | MatLit m        -> let (temp, instrs) = check_matrix m vt
                           in (Matrix, temp, instrs)
      | StrLit s        -> let temp = add_temp String vt
                           in
                               let instrs = [SINI temp;
                                             STR (temp, s)]
                               in (String, temp, instrs)
      | BoolLit b       -> let temp = add_temp Boolean vt
                           in
                               let instrs = [BOOL (temp, BLit b)]
                               in (Boolean, temp, instrs)
      | LValue lv       -> check_lvalue lv vt
      | Plus (e1, e2)   -> let (typ1, temp1, instrs1) = check_expr e1 vt
                           and (typ2, temp2, instrs2) = check_expr e2 vt
                           in
                               if typ1 = Scalar && typ2 = Scalar then
                                   let temp = add_temp Scalar vt
                                   in
                                       let instrs = instrs1 @ instrs2 @
                                                    [ADD (temp, SVar temp1, SVar temp2)]
                                       in (Scalar, temp, instrs)
                               else if typ1 = String && typ2 = String then
                                   let temp = add_temp String vt
                                   in
                                       let instrs = instrs1 @ instrs2 @
                                                    [SCAT (temp, temp1, temp2)]
                                       in (String, temp, instrs)
                               else if typ1 = Matrix && typ2 = Matrix then
                                   let temp = add_temp Matrix vt
                                   in
                                       let instrs = instrs1 @ instrs2 @
                                                    [INIT temp;
                                                     MADD (temp, temp1, temp2)]
                                       in (Matrix, temp, instrs)
                               else if typ1 = Boolean && typ2 = Boolean then
                                   raise WrongDataType
                               else if typ1 = String && typ2 = Scalar then
                                   let temp2prime = add_temp String vt
                                   and temp = add_temp String vt
                                   in
                                       let instrs = instrs1 @ instrs2 @
                                                    [SINI temp2prime;
                                                     SFSC (temp2prime, SVar temp2);
                                                     SINI temp;
                                                     SCAT (temp, temp1, temp2prime)]
                                       in (String, temp, instrs)
                               else if typ1 = Scalar && typ2 = String then
                                   let temp1prime = add_temp String vt
                                   and temp = add_temp String vt
                                   in
                                       let instrs = instrs1 @ instrs2 @
                                                    [SINI temp1prime;
                                                     SFSC (temp1prime, SVar temp1);
                                                     SINI temp;
                                                     SCAT (temp, temp1prime, temp2)]
                                       in (String, temp, instrs)
                               else if typ1 = String && typ2 = Matrix then
                                   let temp2prime = add_temp String vt
                                   and temp = add_temp String vt
                                   in
                                       let instrs = instrs1 @ instrs2 @
                                                    [SINI temp2prime;
                                                     SFMA (temp2prime, temp2);
                                                     SINI temp;
                                                     SCAT (temp, temp1, temp2prime)]
                                       in (String, temp, instrs)
                               else if typ1 = Matrix && typ2 = String then
                                   let temp1prime = add_temp String vt
                                   and temp = add_temp String vt
                                   in
                                       let instrs = instrs1 @ instrs2 @
                                                    [SINI temp1prime;
                                                     SFMA (temp1prime, temp1);
                                                     SINI temp;
                                                     SCAT (temp, temp1prime, temp2)]
                                       in (String, temp, instrs)
                               else
                                   raise TypeMismatch
      | Minus (e1, e2)  -> let (typ1, temp1, instrs1) = check_expr e1 vt
                           and (typ2, temp2, instrs2) = check_expr e2 vt
                           in
                               if typ1 <> typ2 then
                                   raise TypeMismatch
                               else if typ1 = Scalar then
                                   let temp = add_temp Scalar vt
                                   in
                                       let instrs = instrs1 @ instrs2 @
                                                    [SUB (temp, SVar temp1, SVar temp2)]
                                       in (Scalar, temp, instrs)
                               else if typ1 = Matrix then
                                   let temp = add_temp Matrix vt
                                   in
                                       let instrs = instrs1 @ instrs2 @
                                                    [INIT temp;
                                                     MSUB (temp, temp1, temp2)]
                                       in (Matrix, temp, instrs)
                               else
                                   raise WrongDataType
      | Times (e1, e2)  -> let (typ1, temp1, instrs1) = check_expr e1 vt
                           and (typ2, temp2, instrs2) = check_expr e2 vt
                           in
                               if typ1 = Scalar && typ2 = Scalar then
                                   let temp = add_temp Scalar vt
                                   in
                                       let instrs = instrs1 @ instrs2 @
                                                    [MUL (temp, SVar temp1, SVar temp2)]
                                       in (Scalar, temp, instrs)
                               else if typ1 = Matrix && typ2 = Matrix then
                                   let temp = add_temp Matrix vt
                                   in
                                       let instrs = instrs1 @ instrs2 @
                                                    [INIT temp;
                                                     MMUL (temp, temp1, temp2)]
                                       in (Matrix, temp, instrs)
                               else if typ1 = Scalar && typ2 = Matrix then
                                   let temp = add_temp Matrix vt
                                   in
                                       let instrs = instrs1 @ instrs2 @
                                                    [INIT temp;
                                                     SMUL (temp, temp2, SVar temp1)]
                                       in (Matrix, temp, instrs)
                               else if typ1 = Matrix && typ2 = Scalar then
                                   let temp = add_temp Matrix vt
                                   in
                                       let instrs = instrs1 @ instrs2 @
                                                    [INIT temp;
                                                     SMUL (temp, temp1, SVar temp2)]
                                       in (Matrix, temp, instrs)
                               else
                                   raise WrongDataType
      | Divide (e1, e2) -> let (typ1, temp1, instrs1) = check_expr e1 vt
                           and (typ2, temp2, instrs2) = check_expr e2 vt
                           in
                               if typ1 = Scalar && typ2 = Scalar then
                                   let temp = add_temp Scalar vt
                                   in
                                       let instrs = instrs1 @ instrs2 @
                                                    [DIV (temp, SVar temp1, SVar temp2)]
                                       in (Scalar, temp, instrs)
                               else if typ1 = Matrix && typ2 = Scalar then
                                   let temp = add_temp Matrix vt
                                   and temp2prime = add_temp Scalar vt
                                   in
                                       let instrs = instrs1 @ instrs2 @
                                                    [DIV (temp2prime, SLit 1.0, SVar temp2);
                                                     INIT temp;
                                                     SMUL (temp, temp1, SVar temp2prime)]
                                       in (Matrix, temp, instrs)
                               else
                                   raise WrongDataType
      | Power (e1, e2)  -> let (typ1, temp1, instrs1) = check_expr e1 vt
                           and (typ2, temp2, instrs2) = check_expr e2 vt
                           in
                               if typ1 = Scalar && typ2 = Scalar then
                                   let temp = add_temp Scalar vt
                                   in
                                       let instrs = instrs1 @ instrs2 @
                                                    [POW (temp, SVar temp1, SVar temp2)]
                                       in (Scalar, temp, instrs)
                               else if typ1 = Matrix && typ2 = Scalar then
                                   let temp = add_temp Matrix vt
                                   in
                                       let instrs = instrs1 @ instrs2 @
                                                    [INIT temp;
                                                     MPOW (temp, temp1, SVar temp2)]
                                       in (Matrix, temp, instrs)
                               else
                                   raise WrongDataType
      | Eq (e1, e2)     -> let (typ1, temp1, instrs1) = check_expr e1 vt
                           and (typ2, temp2, instrs2) = check_expr e2 vt
                           in
                               if typ1 = Matrix && typ2 = Matrix then
                                   let temp = add_temp Boolean vt
                                   in
                                       let instrs = instrs1 @ instrs2 @
                                                    [SMEQ (temp, temp1, temp2)]
                                       in (Boolean, temp, instrs)
                               else if typ1 = String && typ2 = String then
                                   let cmp_temp = add_temp Scalar vt
                                   and temp = add_temp Boolean vt
                                   in
                                       let instrs = instrs1 @ instrs2 @
                                                    [SCMP (cmp_temp, temp1, temp2);
                                                     SEQ (temp, SVar cmp_temp, SLit 0.0)]
                                       in (Boolean, temp, instrs)
                               else if typ1 = typ2 then
                                   let temp = add_temp Boolean vt
                                   in
                                       let instrs = instrs1 @ instrs2 @
                                                    [SEQ (temp, SVar temp1, SVar temp2)]
                                       in (Boolean, temp, instrs)
                               else
                                   raise TypeMismatch
      | Neq (e1, e2)    -> let (typ1, temp1, instrs1) = check_expr e1 vt
                           and (typ2, temp2, instrs2) = check_expr e2 vt
                           in
                               if typ1 = Matrix && typ2 = Matrix then
                                   let temp = add_temp Boolean vt
                                   in
                                       let instrs = instrs1 @ instrs2 @
                                                    [SMNE (temp, temp1, temp2)]
                                       in (Boolean, temp, instrs)
                               else if typ1 = String && typ2 = String then
                                   let cmp_temp = add_temp Scalar vt
                                   and temp = add_temp Boolean vt
                                   in
                                       let instrs = instrs1 @ instrs2 @
                                                    [SCMP (cmp_temp, temp1, temp2);
                                                     SNE (temp, SVar cmp_temp, SLit 0.0)]
                                       in (Boolean, temp, instrs)
                               else if typ1 = typ2 then
                                   let temp = add_temp Boolean vt
                                   in
                                       let instrs = instrs1 @ instrs2 @
                                                    [SNE (temp, SVar temp1, SVar temp2)]
                                       in (Boolean, temp, instrs)
                               else
                                   raise TypeMismatch
      | Lt (e1, e2)     -> let (typ1, temp1, instrs1) = check_expr e1 vt
                           and (typ2, temp2, instrs2) = check_expr e2 vt
                           in
                               if typ1 <> typ2 then
                                   raise TypeMismatch
                               else if typ1 = String then
                                   let cmp_temp = add_temp Scalar vt
                                   and temp = add_temp Boolean vt
                                   in
                                       let instrs = instrs1 @ instrs2 @
                                                    [SCMP (cmp_temp, temp1, temp2);
                                                     SLT (temp, SVar cmp_temp, SLit 0.0)]
                                       in (Boolean, temp, instrs)
                               else if typ2 = Scalar then
                                   let temp = add_temp Boolean vt
                                   in
                                       let instrs = instrs1 @ instrs2 @
                                                    [SLT (temp, SVar temp1, SVar temp2)]
                                       in (Boolean, temp, instrs)
                               else
                                   raise WrongDataType
      | Gt (e1, e2)     -> let (typ1, temp1, instrs1) = check_expr e1 vt
                           and (typ2, temp2, instrs2) = check_expr e2 vt
                           in
                               if typ1 <> typ2 then
                                   raise TypeMismatch
                               else if typ1 = String then
                                   let cmp_temp = add_temp Scalar vt
                                   and temp = add_temp Boolean vt
                                   in
                                       let instrs = instrs1 @ instrs2 @
                                                    [SCMP (cmp_temp, temp1, temp2);
                                                     SGT (temp, SVar cmp_temp, SLit 0.0)]
                                       in (Boolean, temp, instrs)
                               else if typ1 = Scalar then
                                   let temp = add_temp Boolean vt
                                   in
                                       let instrs = instrs1 @ instrs2 @
                                                    [SGT (temp, SVar temp1, SVar temp2)]
                                       in (Boolean, temp, instrs)
                               else
                                   raise WrongDataType
      | Le (e1, e2)     -> let (typ1, temp1, instrs1) = check_expr e1 vt
                           and (typ2, temp2, instrs2) = check_expr e2 vt
                           in
                               if typ1 <> typ2 then
                                   raise TypeMismatch
                               else if typ1 = String then
                                   let cmp_temp = add_temp Scalar vt
                                   and temp = add_temp Boolean vt
                                   in
                                       let instrs = instrs1 @ instrs2 @
                                                    [SCMP (cmp_temp, temp1, temp2);
                                                     SLE (temp, SVar cmp_temp, SLit 0.0)]
                                       in (Boolean, temp, instrs)
                               else if typ1 = Scalar then
                                   let temp = add_temp Boolean vt
                                   in
                                       let instrs = instrs1 @ instrs2 @
                                                    [SLE (temp, SVar temp1, SVar temp2)]
                                       in (Boolean, temp, instrs)
                               else
                                   raise WrongDataType
      | Ge (e1, e2)     -> let (typ1, temp1, instrs1) = check_expr e1 vt
                           and (typ2, temp2, instrs2) = check_expr e2 vt
                           in
                               if typ1 <> typ2 then
                                   raise TypeMismatch
                               else if typ1 = String then
                                   let cmp_temp = add_temp Scalar vt
                                   and temp = add_temp Boolean vt
                                   in
                                       let instrs = instrs1 @ instrs2 @
                                                    [SCMP (cmp_temp, temp1, temp2);
                                                     SGE (temp, SVar cmp_temp, SLit 0.0)]
                                       in (Boolean, temp, instrs)
                               else if typ1 = Scalar then
                                   let temp = add_temp Boolean vt
                                   in
                                       let instrs = instrs1 @ instrs2 @
                                                    [SGE (temp, SVar temp1, SVar temp2)]
                                       in (Boolean, temp, instrs)
                               else
                                   raise WrongDataType
      | Not e           -> let (typ1, temp1, instrs1) = check_expr e vt
                           in
                               if typ1 <> Boolean then
                                   raise WrongDataType
                               else
                                   let temp = add_temp Boolean vt
                                   in
                                       let instrs = instrs1 @ [NOT (temp, BVar temp1)]
                                       in (Boolean, temp, instrs)
      | And (e1, e2)    -> let (typ1, temp1, instrs1) = check_expr e1 vt
                           and (typ2, temp2, instrs2) = check_expr e2 vt
                           in
                               if typ1 <> Boolean || typ2 <> Boolean then
                                   raise WrongDataType
                               else
                                   let temp = add_temp Boolean vt
                                   in
                                       let instrs = instrs1 @ instrs2 @
                                                    [AND (temp, BVar temp1, BVar temp2)]
                                       in (Boolean, temp, instrs)
      | Or (e1, e2)     -> let (typ1, temp1, instrs1) = check_expr e1 vt
                           and (typ2, temp2, instrs2) = check_expr e2 vt
                           in
                               if typ1 <> Boolean || typ2 <> Boolean then
                                   raise WrongDataType
                               else
                                   let temp = add_temp Boolean vt
                                   in
                                       let instrs = instrs1 @ instrs2 @
                                                    [OR (temp, BVar temp1, BVar temp2)]
                                       in (Boolean, temp, instrs)
      | Trans e         -> let (typ1, temp1, instrs1) = check_expr e vt
                           in
                               if typ1 = Matrix then
                                   let temp = add_temp Matrix vt
                                   in
                                       let instrs = instrs1 @
                                                    [INIT temp;
                                                     TRAN (temp, temp1)]
                                       in (Matrix, temp, instrs)
                               else if typ1 = Scalar then
                                   (Scalar, temp1, instrs1)
                               else
                                   raise WrongDataType
      | SizeRows e      -> let (typ1, temp1, instrs1) = check_expr e vt
                           in
                               if typ1 <> Matrix then
                                   raise WrongDataType
                               else
                                   let temp = add_temp Scalar vt
                                   in
                                       let instrs = instrs1 @
                                                    [ROWS (temp, temp1)]
                                       in (Scalar, temp, instrs)
      | SizeCols e      -> let (typ1, temp1, instrs1) = check_expr e vt
                           in
                               if typ1 <> Matrix then
                                   raise WrongDataType
                               else
                                   let temp = add_temp Scalar vt
                                   in
                                       let instrs = instrs1 @
                                                    [COLS (temp, temp1)]
                                       in (Scalar, temp, instrs)

and check_lvalue lv vt =
    match lv with
        Ident name             -> let n = find_variable name vt
                                  in
                                      let typ = get_variable_type n vt
                                      in (typ, n, [])
      | MAccess (name, e1, e2) -> let n = find_variable name vt
                                  in
                                      let typ0 = get_variable_type n vt
                                      and (typ1, temp1, instrs1) = check_expr e1 vt
                                      and (typ2, temp2, instrs2) = check_expr e2 vt
                                      in
                                          if typ0 <> Matrix || typ1 <> Scalar || typ2 <> Scalar then
                                              raise WrongDataType
                                          else
                                              let temp = add_temp Scalar vt
                                              in
                                                  let instrs = instrs1 @ instrs2 @
                                                               [GET (n, SVar temp1, SVar temp2, temp)]
                                                  in (Scalar, temp, instrs)
      | VAccess (name, e)      -> let n = find_variable name vt
                                  in
                                      let typ0 = get_variable_type n vt
                                      and (typ1, temp1, instrs1) = check_expr e vt
                                      in
                                          if typ0 <> Matrix || typ1 <> Scalar then
                                              raise WrongDataType
                                          else
                                              let temp = add_temp Scalar vt
                                              in
                                                  let instrs = instrs1 @
                                                               [GET (n, SVar temp1, SLit 0.0, temp)]
                                                  in (Scalar, temp, instrs)

let gen_init n typ =
    if typ = Matrix then
        [INIT n]
    else if typ = String then
        [SINI n]
    else
        []

let rec check_stmt s vt ls =
    match s with
        Assign (Ident name, e)             -> let (typr, tempr, instrsr) = check_expr e vt
                                              and n = find_variable name vt
                                              in
                                                  let typ = get_variable_type n vt
                                                  in
                                                      if typ <> typr then
                                                          raise TypeMismatch
                                                      else if typ = Scalar then
                                                          instrsr @ [SCAL (n, SVar tempr)]
                                                      else if typ = Boolean then
                                                          instrsr @ [BOOL (n, BVar tempr)]
                                                      else if typ = String then
                                                          instrsr @ [SCPY (n, tempr)]
                                                      else if typ = Matrix then
                                                          instrsr @ [SMUL (n, tempr, SLit 1.0)]
                                                      else
                                                          raise WrongDataType (* should never happen *)
      | Assign (MAccess (name, e1, e2), e) -> let (typr, tempr, instrsr) = check_expr e vt
                                              and n = find_variable name vt
                                              in
                                                  let typ0 = get_variable_type n vt
                                                  and (typ1, temp1, instrs1) = check_expr e1 vt
                                                  and (typ2, temp2, instrs2) = check_expr e2 vt
                                                  in
                                                      if typ0 <> Matrix || typ1 <> Scalar || typ2 <> Scalar || typr <> Scalar then
                                                          raise WrongDataType
                                                      else
                                                          let instrs = instrs1 @ instrs2 @ instrsr @
                                                                       [SET (n, SVar temp1, SVar temp2, SVar tempr)]
                                                          in instrs
      | Assign (VAccess (name, e1), e)     -> let (typr, tempr, instrsr) = check_expr e vt
                                              and n = find_variable name vt
                                              in
                                                  let typ0 = get_variable_type n vt
                                                  and (typ1, temp1, instrs1) = check_expr e1 vt
                                                  in
                                                      if typ0 <> Matrix || typ1 <> Scalar || typr <> Scalar then
                                                          raise WrongDataType
                                                      else
                                                          let instrs = instrs1 @ instrsr @
                                                                       [SET (n, SVar temp1, SLit 0.0, SVar tempr)]
                                                          in instrs
      | If (e, s1)                         -> let (typc, tempc, instrsc) = check_expr e vt
                                              in
                                                  if typc <> Boolean then
                                                      raise WrongDataType
                                                  else
                                                      let instrst = check_stmt s1 vt ls
                                                      and labend = add_label ls
                                                      in
                                                          instrsc @
                                                          [BRAF (BVar tempc, labend)] @
                                                          instrst @
                                                          [LABL labend]
      | IfElse (e, s1, s2)                 -> let (typc, tempc, instrsc) = check_expr e vt
                                              in
                                                  if typc <> Boolean then
                                                      raise WrongDataType
                                                  else
                                                      let instrst = check_stmt s1 vt ls
                                                      and instrse = check_stmt s2 vt ls
                                                      and labelse = add_label ls
                                                      and labend = add_label ls
                                                      in
                                                          instrsc @
                                                          [BRAF (BVar tempc, labelse)] @
                                                          instrst @
                                                          [JMP labend;
                                                           LABL labelse] @
                                                          instrse @
                                                          [LABL labend]
      | While (e, s1)         -> let (typc, tempc, instrsc) = check_expr e vt
                                 in
                                     if typc <> Boolean then
                                         raise WrongDataType
                                     else
                                         let instrsb = check_stmt s1 vt ls
                                         and labcheck = add_label ls
                                         and labend = add_label ls
                                         in
                                             [LABL labcheck] @
                                             instrsc @
                                             [BRAF (BVar tempc, labend)] @
                                             instrsb @
                                             [JMP labcheck;
                                              LABL labend]
      | Print e               -> let (typ0, temp0, instrs0) = check_expr e vt
                                 in
                                     if typ0 = String then
                                         instrs0 @ [PRINT temp0]
                                     else if typ0 = Scalar then
                                         let temp = add_temp String vt
                                         in
                                             instrs0 @
                                             [SINI temp;
                                              SFSC (temp, SVar temp0);
                                              PRINT temp]
                                     else if typ0 = Matrix then
                                         let temp = add_temp String vt
                                         in
                                             instrs0 @
                                             [SINI temp;
                                              SFMA (temp, temp0);
                                              PRINT temp]
                                     else
                                         raise WrongDataType
      | Dim (name, e1, e2)    -> let n = find_variable name vt
                                 in let typ0 = get_variable_type n vt
                                 and (typ1, temp1, instrs1) = check_expr e1 vt
                                 and (typ2, temp2, instrs2) = check_expr e2 vt
                                 in
                                     if typ0 <> Matrix || typ1 <> Scalar || typ2 <> Scalar then
                                         raise WrongDataType
                                     else
                                         instrs1 @ instrs2 @
                                         [RDIM (n, SVar temp1, SVar temp2)]
      | Decl (t, name)        -> let n = add_variable name t vt
                                 in
                                     gen_init n t
      | DeclInit (t, name, e) -> let n = add_variable name t vt
                                 and (typ1, temp1, instrs1) = check_expr e vt
                                 in
                                     if t <> typ1 then
                                         raise TypeMismatch
                                     else if t = String then
                                         instrs1 @ [SINI n; SCPY (n, temp1)]
                                     else if t = Matrix then
                                         instrs1 @ [INIT n; SMUL (n, temp1, SLit 1.0)]
                                     else if t = Scalar then
                                         instrs1 @ [SCAL (n, SVar temp1)]
                                     else if t = Boolean then
                                         instrs1 @ [BOOL (n, BVar temp1)]
                                     else
                                         raise WrongDataType (* should never happen *)
      | StmtList slist        -> check_prgm slist vt ls

and check_prgm slist vt ls =
    let helper s = check_stmt s vt ls
    and concat_lists l1 l2 = l1 @ l2
    in let list_of_lists = List.map helper slist
    in List.fold_left concat_lists [] list_of_lists
