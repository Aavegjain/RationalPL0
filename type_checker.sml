(* here we add declarations to symbol table and check for types in expressions *)

(* Block
    ([(boolean,"t"),(integer,"y"),(rational,"z"),(rational,"y"),
      (rational,"x")],
     Cmds
       [AssignmentCmd ("x",Rat -),
        AssignmentCmd ("y",BinOp (Plus,Var "z",Bigint "34")),
        PrintCmd (Var "x"),ReadCmd "w",
        ConditionalCmd
          (BinOp (Morethan,Var "x",Rat -),
           Cmds [AssignmentCmd ("y",Bigint "23")],
           Cmds [AssignmentCmd ("y",BinOp (Ratmult,Var "z",Var "z"))]),
        WhileCmd
          (BinOp (Morethanequal,Var "y",Bigint "2345"),
           Cmds
             [ConditionalCmd
                (BinOp (Notequal,Var "x",Bigint "234"),
                 Cmds [AssignmentCmd ("x",BinOp (Ratplus,Var "x",Var "x"))],
                 Cmds [AssignmentCmd ("x",BinOp (Ratmult,Var "x",Var "x"))]),
              AssignmentCmd ("y",BinOp (Minus,Var "y",Bigint "1")),
              AssignmentCmd ("z",BinOp (Plus,Var "z",Bigint "1"))])])
  : DataTypes.Block *)

(* declarations can be inside a block only  *)

structure Type_Checker = struct 
exception Unknown_identifier
exception operator_and_operand_do_not_agree
exception Assignment_of_incorrect_type
exception invalid_operator
exception While_Cmd_Error
exception Conditional_Cmd_Error
exception Procedure_Does_Not_Exist
exception Identifier_not_a_function
exception function_error 
exception Make_Rat_requires_integer_as_arguments 
exception Cannot_print_a_function 
exception Cannot_read_a_procedure


(* returns (val , typ) table tuple *) 
fun helper([], l1 : SymTable.val_symtable , l2 : SymTable.type_symtable) = (rev(l1), rev(l2) ) 
|   helper((t : DataTypes.Typ, v : string) :: tl, l1, l2) = helper(tl, SymTable.symtable_add_var(l1,v), SymTable.typtable_add(l2,v,t)) 
 

(* returns two symbol tables, one  value symtable (all values None) and another type symbol table *)
and add_types(DataTypes.Block(l,lp,DataTypes.Cmds(cmds)) ) = 
        let val valtable : SymTable.val_symtable = [] 
            val typetable : SymTable.type_symtable = [] 
        in 
            helper(l, valtable, typetable) 
        end 

and check_proc(p as  DataTypes.Proc(identifier, DataTypes.ProcBlock(b as DataTypes.Block(l,lp,cmds)) ), typestack, valuestack ) = 
    check_block(b, typestack, valuestack) 
    

and add_proc(p as  DataTypes.Proc(identifier, DataTypes.ProcBlock(b as DataTypes.Block(l,lp,cmds)) ) ,typtable : SymTable.type_symtable, valtable : SymTable.val_symtable, typestack : SymTable.type_symtable list, valuestack : SymTable.val_symtable list) = 
    let val new_t = SymTable.typtable_add(typtable,identifier,DataTypes.proc) 
        val new_v = SymTable.symtable_add(valtable, identifier, DataTypes.ProcVal(p))  
        val (new_v2, new_t2, check) = check_proc(p, new_t :: typestack, new_v :: valuestack) 
         
    in (new_t, new_v)    (* if there is a error in the block then it will be detected automatically in check_block *) 
    end 

and add_proc_helper(lp : DataTypes.Proc list, typtable : SymTable.type_symtable, valtable : SymTable.val_symtable, typestack : SymTable.type_symtable list, valuestack : SymTable.val_symtable list) = 
    if (lp = []) then (typtable, valtable) 
    else let val (new_t, new_v) = add_proc(hd(lp), typtable, valtable, typestack, valuestack)  
         in add_proc_helper(tl(lp), new_t, new_v, typestack, valuestack) 
         end 
         

(* returns a new type table and value table where procedures ae also present *) 
and add_proc_decl(DataTypes.Block(l,lp,DataTypes.Cmds(cmds)), typtable : SymTable.type_symtable, valtable : SymTable.val_symtable, typestack : SymTable.type_symtable list, valuestack : SymTable.val_symtable list)=
    add_proc_helper(lp, typtable, valtable, typestack, valuestack)  



(* returns type of expression if its well typed *)
and check_type_exp( exp : DataTypes.Expression, typestack : SymTable.type_symtable list, valuestack : SymTable.val_symtable list) : DataTypes.Typ = 
    case exp of 
    DataTypes.Rat(r) => DataTypes.rational 
|   DataTypes.Bigint(i) => DataTypes.integer
|   DataTypes.Bool(b) => DataTypes.boolean 
|   DataTypes.Var(s) => (let val (ts,popped) = lookup_t(typestack,[], s)  
                            val (check,tup) = SymTable.typtable_lookup(hd(ts) ,s) 
                         in if check then valOf(tup) else raise Unknown_identifier 
                         end )
|   DataTypes.BinOp(binop, e1, e2) => check_binop_exp(exp, typestack, valuestack) 
|   DataTypes.UnOp(unop, e1) => check_unop_exp(exp, typestack, valuestack) 
|   DataTypes.MakeRat(exp1,exp2) => (let val t1 = check_type_exp(exp1, typestack, valuestack) 
                                        val t2 = check_type_exp(exp2, typestack, valuestack) 
                                    in  if (t1 = DataTypes.integer) andalso (t2 = DataTypes.integer) then DataTypes.rational 
                                        else raise Make_Rat_requires_integer_as_arguments
                                    end 
                                    
                                    )      
(* |  _ => raise invalid_operator *)


and 

check_binop_exp(exp as DataTypes.BinOp(binop, e1, e2), typestack : SymTable.type_symtable list, valuestack : SymTable.val_symtable list) = 
    case binop of 
    DataTypes.Ratplus => if (check_type_exp(e1,typestack, valuestack) = DataTypes.rational andalso  check_type_exp(e2,typestack,valuestack) = DataTypes.rational)
                then DataTypes.rational else raise operator_and_operand_do_not_agree 
|   DataTypes.Ratminus => if (check_type_exp(e1,typestack, valuestack) = DataTypes.rational andalso  check_type_exp(e2,typestack,valuestack) = DataTypes.rational)
                then DataTypes.rational else raise operator_and_operand_do_not_agree 
|    DataTypes.Ratmult => if (check_type_exp(e1,typestack, valuestack) = DataTypes.rational andalso  check_type_exp(e2,typestack,valuestack) = DataTypes.rational)
                then DataTypes.rational else raise operator_and_operand_do_not_agree  
|    DataTypes.Ratdivide =>if (check_type_exp(e1,typestack, valuestack) = DataTypes.rational andalso  check_type_exp(e2,typestack,valuestack) = DataTypes.rational)
                then DataTypes.rational else raise operator_and_operand_do_not_agree 
|   DataTypes.Plus => if (check_type_exp(e1,typestack, valuestack) = DataTypes.integer andalso  check_type_exp(e2,typestack,valuestack) = DataTypes.integer)
                then DataTypes.integer else raise operator_and_operand_do_not_agree 
|   DataTypes.Minus =>  if (check_type_exp(e1,typestack, valuestack) = DataTypes.integer andalso  check_type_exp(e2,typestack,valuestack) = DataTypes.integer)
                then DataTypes.integer else raise operator_and_operand_do_not_agree 
|   DataTypes.Mult =>  if (check_type_exp(e1,typestack, valuestack) = DataTypes.integer andalso  check_type_exp(e2,typestack,valuestack) = DataTypes.integer)
                then DataTypes.integer else raise operator_and_operand_do_not_agree 
|   DataTypes.Divide =>  if (check_type_exp(e1,typestack, valuestack) = DataTypes.integer andalso  check_type_exp(e2,typestack,valuestack) = DataTypes.integer)
                then DataTypes.integer else raise operator_and_operand_do_not_agree 
|   DataTypes.Mod =>  if ( (check_type_exp(e1,typestack, valuestack) = DataTypes.integer) andalso  (check_type_exp(e2,typestack,valuestack) = DataTypes.integer) )
                then DataTypes.integer else raise operator_and_operand_do_not_agree 
|   DataTypes.Andalso =>  if (check_type_exp(e1,typestack, valuestack) = DataTypes.boolean andalso  check_type_exp(e2,typestack,valuestack) = DataTypes.boolean)
                then DataTypes.boolean else raise operator_and_operand_do_not_agree 
|   DataTypes.Orelse => if (check_type_exp(e1,typestack, valuestack) = DataTypes.boolean andalso  check_type_exp(e2,typestack,valuestack) = DataTypes.boolean)
                then DataTypes.boolean else raise operator_and_operand_do_not_agree
|  DataTypes.Equal => (
                        let val t1 =  check_type_exp(e1,typestack, valuestack) 
                            val t2 =  check_type_exp(e2,typestack, valuestack) 
                        in if ( (t1 = DataTypes.rational andalso t2 = DataTypes.rational) orelse 
                                (t1 = DataTypes.integer andalso t2 = DataTypes.integer)  
                                orelse 
                                (t1 = DataTypes.boolean andalso t2 = DataTypes.boolean) ) 
                            then DataTypes.boolean
                            else raise operator_and_operand_do_not_agree
                        end 
                        )
|  DataTypes.Notequal =>(
                        let val t1 =  check_type_exp(e1,typestack, valuestack) 
                            val t2 =  check_type_exp(e2,typestack, valuestack) 
                        in if ( (t1 = DataTypes.rational andalso t2 = DataTypes.rational) orelse 
                                (t1 = DataTypes.integer andalso t2 = DataTypes.integer) 
                                 orelse 
                                (t1 = DataTypes.boolean andalso t2 = DataTypes.boolean) ) 
                            then DataTypes.boolean
                            else raise operator_and_operand_do_not_agree
                        end 
                        )
|  DataTypes.Lessthan =>(
                        let val t1 =  check_type_exp(e1,typestack, valuestack) 
                            val t2 =  check_type_exp(e2,typestack, valuestack) 
                        in if ( (t1 = DataTypes.rational andalso t2 = DataTypes.rational) orelse 
                                (t1 = DataTypes.integer andalso t2 = DataTypes.integer) ) 
                            then DataTypes.boolean
                            else raise operator_and_operand_do_not_agree
                        end 
                        )
|  DataTypes.Morethan => (
                        let val t1 =  check_type_exp(e1,typestack, valuestack) 
                            val t2 =  check_type_exp(e2,typestack, valuestack) 
                        in if ( (t1 = DataTypes.rational andalso t2 = DataTypes.rational) orelse 
                                (t1 = DataTypes.integer andalso t2 = DataTypes.integer) ) 
                            then DataTypes.boolean
                            else raise operator_and_operand_do_not_agree
                        end 
                        )
|  DataTypes.Lessthanequal => (
                        let val t1 =  check_type_exp(e1,typestack, valuestack) 
                            val t2 =  check_type_exp(e2,typestack, valuestack) 
                        in if ( (t1 = DataTypes.rational andalso t2 = DataTypes.rational) orelse 
                                (t1 = DataTypes.integer andalso t2 = DataTypes.integer) ) 
                            then DataTypes.boolean
                            else raise operator_and_operand_do_not_agree
                        end 
                        )
|  DataTypes.Morethanequal => (
                        let val t1 =  check_type_exp(e1,typestack, valuestack) 
                            val t2 =  check_type_exp(e2,typestack, valuestack) 
                        in if ( (t1 = DataTypes.rational andalso t2 = DataTypes.rational) orelse 
                                (t1 = DataTypes.integer andalso t2 = DataTypes.integer) ) 
                            then DataTypes.boolean
                            else raise operator_and_operand_do_not_agree
                        end 
                        )
(* |  _ => raise invalid_operator *)

and check_unop_exp(exp as DataTypes.UnOp(t, e1),  typestack : SymTable.type_symtable list, valuestack : SymTable.val_symtable list ) = 
    case t of 
    DataTypes.Not => (if (check_type_exp(e1,typestack, valuestack) = DataTypes.boolean) then DataTypes.boolean 
                      else raise operator_and_operand_do_not_agree ) 
|   DataTypes.Ratinverse => (if (check_type_exp(e1,typestack,valuestack) = DataTypes.rational) then DataTypes.rational 
                      else raise operator_and_operand_do_not_agree ) 
|   DataTypes.unminus => ( if (check_type_exp(e1,typestack,valuestack) = DataTypes.rational) then DataTypes.rational 
                        else if (check_type_exp(e1,typestack,valuestack) = DataTypes.integer) then DataTypes.integer  
                      else raise operator_and_operand_do_not_agree 
                )
(* |  _ => raise invalid_operator *)

and lookup_t(typestack : SymTable.type_symtable list, poppedstack : SymTable.type_symtable list, identifier) = 
    if (typestack = []) then raise Unknown_identifier 
    else let val (check,t2) = SymTable.typtable_lookup(hd(typestack),identifier) 
         in if (check)  then (typestack, poppedstack) 
            else lookup_t(tl(typestack), hd(typestack) :: poppedstack, identifier)  
         end 

and lookup_v(valuestack : SymTable.val_symtable list, poppedstack : SymTable.val_symtable list, identifier) = 
    if (valuestack = []) then raise Unknown_identifier 
    else let val (check,t2) = SymTable.symtable_lookup(hd(valuestack),identifier) 
         in if (check)  then (valuestack, poppedstack) 
            else lookup_v(tl(valuestack), hd(valuestack) :: poppedstack, identifier)  
         end 

and check_cmd(cmd : DataTypes.Cmd, typestack : SymTable.type_symtable list, valuestack : SymTable.val_symtable list) = 
    case cmd of 
    DataTypes.AssignmentCmd(identifier, exp) => (
                                        let 
                                            val t1 = check_type_exp(exp,typestack,valuestack) 
                                            val (ts,popped) = lookup_t(typestack, [], identifier)
                                            (* if it returns then no error is raised and the hd of typs has my identifier *)
                                            val (check, t2) = SymTable.typtable_lookup(hd(ts), identifier) 
                                        in 
                                            if (check) then 
                                                if (valOf(t2) = t1) then true else raise Assignment_of_incorrect_type 
                                            else raise Unknown_identifier 
                                        end 
                                    )
|   DataTypes.ReadCmd(identifier) => (let 
                                           
                                            val (ts,popped) = lookup_t(typestack, [], identifier)
                                            (* if it returns then no error is raised and the hd of typs has my identifier *)
                                            val (check, t2) = SymTable.typtable_lookup(hd(ts), identifier) 
                                         in if (check) then     
                                                            if (valOf(t2) <> DataTypes.proc) then  true 
                                                            else raise Cannot_read_a_procedure
                                            else raise Unknown_identifier
                                      end 
                                    )
|   DataTypes.PrintCmd(exp) => let val t = check_type_exp(exp,typestack, valuestack)  
                                in if ( (t = DataTypes.rational) orelse (t = DataTypes.integer) orelse (t = DataTypes.boolean) ) then true 
                                    else raise Cannot_print_a_function
                                end 
|   DataTypes.WhileCmd(exp, cmds) => if ( (check_type_exp(exp,typestack,valuestack) = DataTypes.boolean) andalso check_cmds(cmds, typestack, valuestack) ) 
                                        then true else raise While_Cmd_Error
|   DataTypes.ConditionalCmd(exp,cmds1, cmds2) => if ( (check_type_exp(exp,typestack, valuestack) = DataTypes.boolean) andalso check_cmds(cmds1, typestack, valuestack) andalso check_cmds(cmds2, typestack, valuestack) ) 
                                                    then true else raise Conditional_Cmd_Error
|   DataTypes.CallCmd(identifier) => (let  val (ts,popped) = lookup_t(typestack, [], identifier)  
                                           val (check,t2) = SymTable.typtable_lookup(hd(ts),identifier) 
                                      in if (check) then 
                                                    if (valOf(t2) = DataTypes.proc) then 
                                                                                        let val (vs,popped) = lookup_v(valuestack,[], identifier) 
                                                                                            val (check2, ast) = SymTable.symtable_lookup(hd(vs), identifier)
                                                                                            
                                                                                        in if (check2) then true else raise Unknown_identifier (* will never be raised as lookkup_v will raise error if no such fn present *) 
                                                                                        end 
                                                    else raise Identifier_not_a_function 
                                        else raise Unknown_identifier

                                      end )

(* |  _ => raise invalid_operator *)



and check_cmds(DataTypes.Cmds(cmd_list), typestack : SymTable.type_symtable list, valuestack : SymTable.val_symtable list)  = 
    let fun f(cmd : DataTypes.Cmd, x2 : bool)  = check_cmd(cmd,typestack,valuestack) andalso x2 
    in List.foldl(f) true cmd_list  
    end 

(* this function adds all the declarations - variables and procedures - and returns (new_t,new_v) 
        where the tuple has the new bindings used to check the commands which follow *) 
and check_block(b as DataTypes.Block(l,lp,cmds),typestack, valuestack)  = 
    let val (l1,l2) = add_types(b) 
        val (new_t, new_v) = add_proc_decl(b, l2, l1, typestack, valuestack )  

    in (new_v,new_t,check_cmds(cmds, new_t :: typestack, new_v :: valuestack))
    end 
    
end ; 