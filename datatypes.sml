

signature DATATYPES = 
sig 
  datatype Boolean = tt | ff 
  eqtype integer
  eqtype rational 
  datatype Ident = Ident of string 
  and BinOp = Ratplus | Ratminus | Ratmult | Ratdivide | Plus | Minus | Mult | Divide | Mod 
              | Andalso | Orelse | Equal | Notequal | Lessthan | Morethan | Lessthanequal | Morethanequal 
and UnOp =  Not | Ratinverse  | unminus 
  and Expression = Rat of Rational.rational | Bigint of Bigint.bigint | Bool of Boolean | Var of string | 
                   BinOp of BinOp * Expression * Expression | UnOp of UnOp * Expression | MakeRat of Expression * Expression 
  
  and Typ = rational | integer | boolean | proc  
  and Decl = Decl  of string list * Typ  ; 
  datatype Cmd = AssignmentCmd of string * Expression | ReadCmd of string |PrintCmd of Expression | CallCmd of string 
                | WhileCmd of Expression * Cmds | ConditionalCmd of Expression * Cmds * Cmds  
  and Cmds = Cmds of (Cmd list) 
  and Proc = Proc of string * ProcBlock 

  and Block = Block of ((Typ * string) list *  Proc list  * Cmds) 
  and ProcBlock = ProcBlock of Block 

  and myval =  RatVal of rational | IntVal of integer | BoolVal of Boolean | ProcVal of Proc
  
 

   

end;


structure DataTypes : DATATYPES = 
struct 
  datatype Boolean = tt | ff
  type integer = Bigint.bigint 
  type rational = Rational.rational 

datatype Ident = Ident of string 
  and BinOp = Ratplus | Ratminus | Ratmult | Ratdivide | Plus | Minus | Mult | Divide | Mod 
              | Andalso | Orelse | Equal | Notequal | Lessthan | Morethan | Lessthanequal | Morethanequal 
  and UnOp =  Not | Ratinverse  | unminus 
  and Expression = Rat of Rational.rational | Bigint of Bigint.bigint | Bool of Boolean | Var of string | 
                   BinOp of BinOp * Expression * Expression | UnOp of UnOp * Expression | MakeRat of Expression * Expression 
  
  and Typ = rational | integer | boolean | proc  
  and Decl = Decl  of string list * Typ  ;

  datatype Cmd = AssignmentCmd of string * Expression | ReadCmd of string |PrintCmd of Expression | CallCmd of string 
                | WhileCmd of Expression * Cmds | ConditionalCmd of Expression * Cmds * Cmds  
  and Cmds = Cmds of (Cmd list) 
  and Proc = Proc of string * ProcBlock 

  and Block = Block of ((Typ * string) list *  Proc list  * Cmds)   
  and ProcBlock = ProcBlock of Block 

  and myval =  RatVal of rational | IntVal of integer | BoolVal of Boolean | ProcVal of Proc 

    
  


end;


(* old grammar 
datatype Ident = Ident of string 
  and Rational_Node = Rational_Node of Rational.rational
  and Bigint_Node = Bigint_Node of Bigint.bigint 
  and Boolean_Node = Boolean_Node of boolean 
  and Node = NodeVar of Ident | NodeRat of Rational_Node | NodeInt of Bigint_Node | NodeBool of Boolean_Node 
  and BinRatOp = BinRatOp of string * RatExp * RatExp 
  and BinIntOp = BinIntOp of string * IntExp * IntExp 
  and BinBoolOp = BinBoolOp of string * BoolExp * BoolExp 
  and RatIntExp = RatIntExp1 of RatExp | RatIntExp2 of IntExp  
  and RelOp = RelOp of string * RatIntExp * RatIntExp 
  and RatExp = RatExpBin of BinRatOp | RatExpUn of UnRatOp 
  and UnRatOp = UnRatOp of string * Node 
  and IntExp = IntExpBin of BinIntOp | IntExpUn of UnIntOp 
  and UnIntOp = UnIntOp of string * Node  (* do type checking when making this node *) 
  and BoolExp = BoolExpBin of BinBoolOp | BoolExpUn of UnBoolOp 
  and UnBoolOp = UnBoolOp of string * Node 
  and Expression = ExpressionRat of RatExp | ExpressionBool of BoolExp | ExpressionInt of IntExp 
  
  and PrintCmd = PrintCmd of Expression 
  and ReadCmd = ReadCmd of Ident 
  and AssignmentCmd = AssignmentCmd of Ident * Expression 

  and Cmd = CmdAss of AssignmentCmd | CmdRead of ReadCmd | CmdPrint of PrintCmd  *)


  (* (* and value = RatVal of rational | IntVal of integer | BoolVal of Boolean  *)

  and val_symtable  =ident * value list 
  and type_symtable = ident * Typ list *)
