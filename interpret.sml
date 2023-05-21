CM.make("$/basis.cm") ;
CM.make("$/ml-yacc-lib.cm") ;
use "bigint.sml" ;
use "rational_structure.sml" ;
use "datatypes.sml" ;
use "bool.sml" ; 
use "symbol_table.sml" ;
use "type_checker.sml" ;
use "evaluator.sml" ;
use "while.grm.sig" ;
use "while.grm.sml" ;
use "while.lex.sml" ;
use "glue.sml" ;


(* structure While :
sig val interpret : string * string -> bool 
end =
struct *)
exception WhileError;
fun interpret(inputFile, outputFile) =
    let val inStream = TextIO.openIn inputFile; 
        val outStream = TextIO.openOut outputFile ; 
        val grab : int -> string = fn
            n => if TextIO.endOfStream inStream
                 then ""
                 else TextIO.inputN (inStream,n);
        val printError : string * int * int -> unit = fn
            (msg,line,col) =>
            print (inputFile^"["^Int.toString line^":"
                ^Int.toString col^"] "^msg^"\n");
        val (tree,rem) = WhileParser.parse
                        (15,    (* error correction tokens count *) 
                        (WhileParser.makeLexer grab inputFile),
                        printError,
                        inputFile)
            handle WhileParser.ParseError => raise WhileError;
            (* Close the source program file *)
        val _ = TextIO.closeIn inStream;
        val (new_v, new_t, check_block) = Type_Checker.check_block(tree,[],[])
        val u = Evaluator.eval_block(tree, new_t :: [] , new_v :: [], outStream) 
        val _ = TextIO.closeOut outStream ; 
    in check_block 
    
end
(* end; *)

(* Control.Print.printLength := 10000; 
Control.Print.printDepth := 10000; 
Control.Print.stringDepth := 10000;  *)