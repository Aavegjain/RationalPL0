(* program run after static semantics i.e type checking is performed  *)
structure Evaluator = 
struct 
exception uninitialised_variable
exception Division_By_Rat_Zero
exception invalid_input 

fun get_rat_val(DataTypes.RatVal(x)) = x 
and get_int_val(DataTypes.IntVal(x)) = x 
and get_bool_val(DataTypes.BoolVal(x)) = x 



and eval_exp(exp : DataTypes.Expression, typestack : SymTable.type_symtable list, valuestack : SymTable.val_symtable list) : DataTypes.myval = 
    case exp of 
    DataTypes.Rat(r) => DataTypes.RatVal(r)
|   DataTypes.Bigint(i) => DataTypes.IntVal(i)
|   DataTypes.Bool(b) => DataTypes.BoolVal(b)  
|   DataTypes.Var(s) => (let val (ts, poppedstack) =lookup_v(valuestack,[],s)
                            val (check,option) = SymTable.symtable_lookup(hd(ts),s) 
                         in if (check) then  
                                case valOf(option) of 
                                    SOME(x) => x 
                                |   NONE => raise uninitialised_variable 
                            else raise Type_Checker.Unknown_identifier 
                         end )
|   DataTypes.BinOp(binop, e1, e2) => eval_binop_exp(exp, typestack, valuestack)   
|   DataTypes.UnOp(unop, e1) => eval_unop_exp(exp, typestack, valuestack)  
|   DataTypes.MakeRat(exp1,exp2) => (let
                                        val o1 = get_int_val(eval_exp(exp1, typestack, valuestack)) 
                                        val o2 = get_int_val(eval_exp(exp2, typestack, valuestack)) 
                                        val option  = Rational.make_rat(o1,o2) 
                                      in case option of 
                                        SOME(v) => DataTypes.RatVal(v)
                                      |  NONE => raise Division_By_Rat_Zero
                                    end 
                                      )
                                      
                                        
                                      

and 


(* no type checking involved now as Type_Checker already took care of that *)
eval_binop_exp(exp as DataTypes.BinOp(binop,e1,e2) : DataTypes.Expression, typestack : SymTable.type_symtable list, valuestack : SymTable.val_symtable list) : DataTypes.myval = 
    case binop of 
    DataTypes.Ratplus => DataTypes.RatVal(Rational.add(get_rat_val(eval_exp(e1,typestack, valuestack)), get_rat_val(eval_exp(e2,typestack,valuestack)) ) )
|   DataTypes.Ratminus => DataTypes.RatVal(Rational.subtract(get_rat_val(eval_exp(e1,typestack,valuestack)), get_rat_val(eval_exp(e2,typestack,valuestack)) ) )
|   DataTypes.Ratmult => DataTypes.RatVal(Rational.multiply(get_rat_val(eval_exp(e1,typestack,valuestack)), get_rat_val(eval_exp(e2,typestack,valuestack)) ) )
|   DataTypes.Ratdivide => (case Rational.divide(get_rat_val(eval_exp(e1,typestack,valuestack)), get_rat_val(eval_exp(e2,typestack,valuestack)) ) of  
                                SOME(x) => DataTypes.RatVal(x) 
                            |   NONE => raise Division_By_Rat_Zero
                                )
|   DataTypes.Plus => DataTypes.IntVal(Bigint.add(get_int_val(eval_exp(e1,typestack,valuestack)), get_int_val(eval_exp(e2,typestack,valuestack)) ) )
|   DataTypes.Minus => DataTypes.IntVal(Bigint.sub(get_int_val(eval_exp(e1,typestack,valuestack)), get_int_val(eval_exp(e2,typestack,valuestack)) ) )
|   DataTypes.Mult => DataTypes.IntVal(Bigint.mult(get_int_val(eval_exp(e1,typestack,valuestack)), get_int_val(eval_exp(e2,typestack,valuestack)) ) )
|   DataTypes.Divide => DataTypes.IntVal(#1 (Bigint.divide(get_int_val(eval_exp(e1,typestack,valuestack)), get_int_val(eval_exp(e2,typestack,valuestack)) )) ) 
|   DataTypes.Mod => DataTypes.IntVal( #2 (Bigint.divide(get_int_val(eval_exp(e1,typestack,valuestack)), get_int_val(eval_exp(e2,typestack,valuestack)) )) )

|   DataTypes.Andalso => DataTypes.BoolVal(Bool.bool_and(get_bool_val(eval_exp(e1,typestack,valuestack)), get_bool_val(eval_exp(e2,typestack,valuestack)) ) )
|   DataTypes.Orelse => DataTypes.BoolVal(Bool.bool_or(get_bool_val(eval_exp(e1,typestack,valuestack)), get_bool_val(eval_exp(e2,typestack,valuestack)) ) )
|   DataTypes.Equal => (case Type_Checker.check_type_exp(e1,typestack,valuestack) of 
                        DataTypes.rational => let val b = Rational.equal(get_rat_val(eval_exp(e1,typestack,valuestack)), get_rat_val(eval_exp(e2,typestack,valuestack)) ) in 
                                                if (b) then DataTypes.BoolVal(DataTypes.tt) else DataTypes.BoolVal(DataTypes.ff) 
                                              end 
                    |    DataTypes.integer => let val b = Bigint.equal(get_int_val(eval_exp(e1,typestack,valuestack)), get_int_val(eval_exp(e2,typestack,valuestack)) ) in 
                                                if (b) then DataTypes.BoolVal(DataTypes.tt) else DataTypes.BoolVal(DataTypes.ff) 
                                              end 
                    |    DataTypes.boolean => let val b = Bool.equal(get_bool_val(eval_exp(e1,typestack,valuestack)), get_bool_val(eval_exp(e2,typestack,valuestack)) ) in 
                                                if (b) then DataTypes.BoolVal(DataTypes.tt) else DataTypes.BoolVal(DataTypes.ff) 
                                              end 
                    |  _ => raise Type_Checker.operator_and_operand_do_not_agree)   (* wont occur due to type checking *) 

|   DataTypes.Notequal => (case Type_Checker.check_type_exp(e1,typestack,valuestack) of 
                        DataTypes.rational => let val b = Rational.equal(get_rat_val(eval_exp(e1,typestack,valuestack)), get_rat_val(eval_exp(e2,typestack,valuestack)) ) in 
                                                if (b) then DataTypes.BoolVal(DataTypes.ff) else DataTypes.BoolVal(DataTypes.tt) 
                                              end 
                    |    DataTypes.integer => let val b = Bigint.equal(get_int_val(eval_exp(e1,typestack,valuestack)), get_int_val(eval_exp(e2,typestack,valuestack)) ) in 
                                                if (b) then DataTypes.BoolVal(DataTypes.ff) else DataTypes.BoolVal(DataTypes.tt) 
                                              end 
                     |    DataTypes.boolean => let val b = Bool.equal(get_bool_val(eval_exp(e1,typestack,valuestack)), get_bool_val(eval_exp(e2,typestack,valuestack)) ) in 
                                                if (b) then DataTypes.BoolVal(DataTypes.ff) else DataTypes.BoolVal(DataTypes.tt) 
                                              end 
                    |  _ => raise Type_Checker.operator_and_operand_do_not_agree )  (* wont occur due to type checking *) 

|   DataTypes.Lessthan => (case Type_Checker.check_type_exp(e1,typestack,valuestack) of 
                        DataTypes.rational => let val b = Rational.less(get_rat_val(eval_exp(e1,typestack,valuestack)), get_rat_val(eval_exp(e2,typestack,valuestack)) ) in 
                                                if (b) then DataTypes.BoolVal(DataTypes.tt) else DataTypes.BoolVal(DataTypes.ff) 
                                              end 
                    |    DataTypes.integer => let val b = Bigint.less(get_int_val(eval_exp(e1,typestack,valuestack)), get_int_val(eval_exp(e2,typestack,valuestack)) ) in 
                                                if (b) then DataTypes.BoolVal(DataTypes.tt) else DataTypes.BoolVal(DataTypes.ff) 
                                              end 
                    |  _ => raise Type_Checker.operator_and_operand_do_not_agree)   (* wont occur due to type checking *) 

|   DataTypes.Morethan => (case Type_Checker.check_type_exp(e1,typestack,valuestack) of 
                        DataTypes.rational => let val b = Rational.less(get_rat_val(eval_exp(e2,typestack,valuestack)), get_rat_val(eval_exp(e1,typestack,valuestack))) in 
                                                if (b) then DataTypes.BoolVal(DataTypes.tt) else DataTypes.BoolVal(DataTypes.ff) 
                                              end 
                    |    DataTypes.integer => let val b = Bigint.less(get_int_val(eval_exp(e2,typestack,valuestack)), get_int_val(eval_exp(e1,typestack,valuestack))) in 
                                                if (b) then DataTypes.BoolVal(DataTypes.tt) else DataTypes.BoolVal(DataTypes.ff) 
                                              end 
                    |  _ => raise Type_Checker.operator_and_operand_do_not_agree)   (* wont occur due to type checking *) 

|   DataTypes.Lessthanequal => (case Type_Checker.check_type_exp(e1,typestack,valuestack) of 
                        DataTypes.rational => let val b = Rational.less(get_rat_val(eval_exp(e2,typestack,valuestack)), get_rat_val(eval_exp(e1,typestack,valuestack))) in 
                                                if (b) then DataTypes.BoolVal(DataTypes.ff) else DataTypes.BoolVal(DataTypes.tt) 
                                              end 
                    |    DataTypes.integer => let val b = Bigint.less(get_int_val(eval_exp(e2,typestack,valuestack)), get_int_val(eval_exp(e1,typestack,valuestack))) in 
                                                if (b) then DataTypes.BoolVal(DataTypes.ff) else DataTypes.BoolVal(DataTypes.tt) 
                                              end 
                    |  _ => raise Type_Checker.operator_and_operand_do_not_agree  ) (* wont occur due to type checking *) 

|   DataTypes.Morethanequal => (case Type_Checker.check_type_exp(e1,typestack,valuestack) of 
                        DataTypes.rational => let val b = Rational.less(get_rat_val(eval_exp(e1,typestack,valuestack)), get_rat_val(eval_exp(e2,typestack,valuestack)) ) in 
                                                if (b) then DataTypes.BoolVal(DataTypes.ff) else DataTypes.BoolVal(DataTypes.tt) 
                                              end 
                    |    DataTypes.integer => let val b = Bigint.less(get_int_val(eval_exp(e1,typestack,valuestack)), get_int_val(eval_exp(e2,typestack,valuestack)) ) in 
                                                if (b) then DataTypes.BoolVal(DataTypes.ff) else DataTypes.BoolVal(DataTypes.tt) 
                                              end 
                    |  _ => raise Type_Checker.operator_and_operand_do_not_agree  ) (* wont occur due to type checking *) 

and eval_unop_exp(exp as DataTypes.UnOp(t, e1), typestack : SymTable.type_symtable list,valuestack : SymTable.val_symtable list) : DataTypes.myval = 
    case t of  
    DataTypes.Not =>DataTypes.BoolVal(Bool.bool_not(get_bool_val(eval_exp(e1,typestack,valuestack)))) 
|   DataTypes.Ratinverse =>  (case Rational.inverse(get_rat_val(eval_exp(e1,typestack,valuestack))) of  
                                SOME(x) =>DataTypes.RatVal(x) 
                            |   NONE => raise Division_By_Rat_Zero)
|   DataTypes.unminus => ( if (Type_Checker.check_type_exp(e1,typestack,valuestack) = DataTypes.integer) then DataTypes.IntVal( Bigint.negate(get_int_val(eval_exp(e1,typestack,valuestack))) )
                        else if (Type_Checker.check_type_exp(e1,typestack,valuestack) = DataTypes.rational) then DataTypes.RatVal( Rational.neg(get_rat_val(eval_exp(e1,typestack,valuestack))))
                      else raise Type_Checker.operator_and_operand_do_not_agree 
                )
                                

and lookup_t(typestack : SymTable.type_symtable list, poppedstack : SymTable.type_symtable list, identifier) = 
    if (typestack = []) then raise Type_Checker.Unknown_identifier 
    else let val (check,t2) = SymTable.typtable_lookup(hd(typestack),identifier) 
         in if (check)  then (typestack, poppedstack) 
            else lookup_t(tl(typestack), hd(typestack) :: poppedstack, identifier)  
         end 

and lookup_v(valuestack : SymTable.val_symtable list, poppedstack : SymTable.val_symtable list, identifier) = 
    if (valuestack = []) then raise Type_Checker.Unknown_identifier 
    else let val (check,t2) = SymTable.symtable_lookup(hd(valuestack),identifier) 
         in if (check)  then (valuestack, poppedstack) 
            else lookup_v(tl(valuestack), hd(valuestack) :: poppedstack, identifier)  
         end 
and extend(l1, []) = l1 | 
    extend(l1, hd :: tl ) = extend(hd :: l1, tl) 
  
and extend2(l1, []) = l1 | 
    extend2(l1, hd :: tl ) = extend2(hd :: l1, tl)

and eval_cmd(cmd : DataTypes.Cmd, typestack: SymTable.type_symtable list, valuestack : SymTable.val_symtable list, outstream) = 
    case cmd of 
    DataTypes.AssignmentCmd(identifier, exp) => (
                                        let 
                                            val v = eval_exp(exp,typestack,valuestack) 
                                            val (ts, poppedstack) =lookup_v(valuestack,[],identifier) 
                                            val new_vt = SymTable.symtable_update(hd(ts), identifier, v)
                                            val new_valuestack = extend(new_vt :: tl(ts), poppedstack) 
                                        in 
                                           (typestack, new_valuestack)  
                                        end 
                                    )
|   DataTypes.ReadCmd(identifier) => (let 
                                        val inputoption = TextIO.inputLine TextIO.stdIn
                                        val inputText = case inputoption of 
                                                        NONE => raise invalid_input 
                                                      | SOME(T) => String.substring(T, 0, String.size(T) - 1 )
                                        val (ts2,popped2) = lookup_t(typestack, [], identifier)
                                            (* if it returns then no error is raised and the hd of typs has my identifier *)
                                        val (check, t2) = SymTable.typtable_lookup(hd(ts2), identifier) 
                                      
                                        val v = ( case valOf(t2) of 
                                                     DataTypes.rational =>  DataTypes.RatVal(Rational.fromDecimal(inputText)) 
                                                  |  DataTypes.integer => DataTypes.IntVal(Bigint.make_from_str(inputText)) 
                                                  |  DataTypes.boolean => DataTypes.BoolVal(Bool.make_from_str(inputText)) 
                                                |  _ => raise Type_Checker.Cannot_read_a_procedure ) 
                                        val (ts, poppedstack) =lookup_v(valuestack,[],identifier) 
                                        val new_vt = SymTable.symtable_update(hd(ts), identifier, v)
                                        val new_valuestack = extend(new_vt :: tl(ts), poppedstack) 
 
                                      in (typestack, new_valuestack) 
                                      end 
                                    )
|   DataTypes.PrintCmd(exp) => let val v = eval_exp(exp,typestack,valuestack)  
                                    val t = Type_Checker.check_type_exp(exp, typestack, valuestack) 
                                in case t of 
                                    DataTypes.rational => ( let val d = TextIO.output(outstream, Rational.showDecimal(get_rat_val(v)) ^ "\n") 
                                                            in (typestack,valuestack) 
                                                            end )
                                |   DataTypes.integer =>  ( let val d = TextIO.output(outstream, get_int_val(v) ^ "\n") 
                                                            in (typestack,valuestack)  
                                                            end )
                                |   DataTypes.boolean => ( let val d = TextIO.output(outstream, Bool.bool_to_str(get_bool_val(v)) ^ "\n") 
                                                            in (typestack,valuestack) 
                                                            end )
                                |  _ => raise Type_Checker.Cannot_print_a_function
                                end 
|  DataTypes.ConditionalCmd(exp,cmds1, cmds2) => let val check = get_bool_val(eval_exp(exp,typestack,valuestack)) 
                                                 in if (check = DataTypes.tt) then eval_cmds(cmds1,typestack,valuestack,outstream) 
                                                    else eval_cmds(cmds2,typestack,valuestack,outstream) 
                                                end 
|   DataTypes.WhileCmd(exp, cmds) => let val check = get_bool_val(eval_exp(exp,typestack,valuestack))  
                                     in if (check = DataTypes.ff) then (typestack,valuestack) 
                                        else let val (new_t,new_v) = eval_cmds(cmds,typestack,valuestack,outstream) 
                                             in eval_cmd(cmd,new_t, new_v,outstream) 
                                             end 
                                    end 
|  DataTypes.CallCmd(identifier) => let val (ts, poppedstack) =lookup_v(valuestack,[],identifier)
                                        val (ts2,poppedstack2) = lookup_t(typestack, [], identifier)
                                        val (check,SOME(SOME(DataTypes.ProcVal(DataTypes.Proc(identifier, DataTypes.ProcBlock(b as DataTypes.Block(l,lp,cmds))))))) = SymTable.symtable_lookup(hd(ts),identifier) 
                                        (* can assume SOME as type checking already performed *) 
                                        val (new_v, new_t, check2) = Type_Checker.check_block(b,  ts2, ts) 
                                        val (new_t_stack, new_v_stack) = eval_cmds(cmds, new_t :: ts2, new_v :: ts,outstream) 
                                        (* pushing on the call stack *) 
                                    (* in (tl(new_t_stack), tl(new_v_stack)) popping off the call stack  *)
                                    in (extend2(tl(new_t_stack), poppedstack2) , extend(tl(new_v_stack),poppedstack))  
                                    end 


and eval_cmds(DataTypes.Cmds(cmd_list), typestack : SymTable.type_symtable list , valuestack : SymTable.val_symtable list,outstream) = 
    if (cmd_list = []) then (typestack,valuestack) 
    else let val (new_t_stack, new_v_stack) = eval_cmd(hd(cmd_list), typestack,valuestack,outstream) 
         in eval_cmds(DataTypes.Cmds(tl(cmd_list)), new_t_stack, new_v_stack,outstream) 
         end 

and eval_block(b as DataTypes.Block(l,lp,cmds), typestack : SymTable.type_symtable list, valuestack : SymTable.val_symtable list, outstream  ) = 
    eval_cmds(cmds, typestack, valuestack, outstream) 

end ; 

