In this assignment we implement our own imperative programming language - RationalPL0. 

Salient features- 
1. the language has three datatypes - boolean, Bigint, Rational. Bigint is the datatype of integers
with arbitrary number of significant digits, same is true for Rational. boolean can take only two values-- tt , ff
2. For knowing the syntax and semantics of the language, please refer the attached pdf. (grammar at the end of the README.md). 
3. The language has static scoping as is common for most programming languages nowadays.
4. The language has support for parameterless procedures only. 
5. The language also supports recursion. 
6. Type checking performed before runtime. Thus statically typed. 
7. Exception handling has been implemented in light of type checking, syntax, semantics and 
valid arithmetic operations.  

Instructions to use the language-
1. Download the folder and launch the smlnj interpreter. 
2. If you have ml-lex installed, run - "ml-lex while.lex" . this will generate a new file -
while.lex.sml If you dont have ml-lex, you can use the version of while.lex.sml in the downloaded
folder.
3. If you have ml-yacc installed, run - "ml-yacc while.grm" . this will generate two new files -
while.grm.sml and while.grm.sig If you dont have ml-yacc, you can use the version of while.grm.sml 
and while.grm.sig in the downloaded folder.
4. run the file interpret.sml .
5. for a file in the RationalPL0 language named <name>.rat and output file named 
<output> run the function - "interpret("<name>.rat", <output>) 
6. the output will be written to a new file generated named - <output>. 
7. Some sample test files have been added in the tests folder. 


Design decisions - 
0. Run interpret.sml in the terminal. Then use the function "inetrpret" as given in specifications.  

1. in same scope,  we cannot have two variable names with different types but same names as
     it will cause weird behaviour. (may raise unneccessary type error )

2. above holds for a procedure and a variable having same name also . 

3. refer while.grm for precedence used

4. make rat, rat, showRat, showDecimal, fromDecimal, toDecimal , 
rational, integer, boolean, tt, ff, var, if, then, else, fi, while, do, od procedure, print, read, call, inverse are reserved words. Of these var, showRat, showDecimal, toDecimal, rat are 
not available as commands to the user. 

5. 1 +3 -- incorrect ; 1 + 3 -- correct (lexer issue) 

6. proper exception handling has been done. 

7. assuming only one line of input is given in read cmd which can only be a rational in std dec form, integer or boolean(tt,ff) . 

8. no rat present - use makerat 

9. unary plus faulty as +(Expression) not allowed 
 
Grammar used for parsing - 

convention followed -- terminals in small letters, non terminals in capital letters
"epsilon" has usual meaning .
terminals represent their name, except for lparen  - (, rparen  - ), curlylparen - {, curlyrparen - } 
comma - ",", semicolon - ";", assign - ":=" , 
+ - plus, "-" - minus, * - mult , / - divide, .+. - ratplus, .-. - ratminus, .*. - ratmult, ./. - ratdivide , % - mod, = - equal, <> - notequal, > - morethan, >= - morethanequal , <= - lessthanequal, < - lessthan, "inverse" - ratinverse , "~" - unaryminus(unminus) 

PROGRAM -> BLOCK 
BLOCK -> DECLARATIONSEQ COMMANDSEQ 
DECLARATIONSEQ -> VARDECLS PROCDECLS 
VARDECLS ->  RATVARDECLS | INTVARDECLS | BOOLVARDECLS 
RATVARDECLS -> epsilon | rational IDENT IDENTLIST semicolon
INTVARDECLS -> epsilon | integer IDENT IDENTLIST semicolon
BOOLVARDECLS -> epsilon | boolean IDENT IDENTLIST semicolon  
IDENTLIST -> epsilon | comma IDENT IDENTLIST 
PROCDECLS ->epsilon | PROCDEF semicolon PROCDECLS 
PROCDEF ->PROCEDURE IDENT BLOCK
COMMANDSEQ -> curlylparen COMMANDS  curlyrightparen 
COMMANDS -> epsilon | COMMAND semicolon COMMANDS 
COMMAND -> ASSIGNMENTCMD | READCMD | PRINTCMD | CALLCMD | WHILECMD | CONDITIONALCMD | CALLCMD 
ASSIGNMENTCMD ->IDENT assign EXPRESSION 
READCMD ->read lparen IDENT rparen 
PRINTCMD ->print lparen EXPRESSION rparen  
CALLCMD ->call IDENT 
WHILECMD ->while EXPRESSION do COMMANDSEQ od 
CONDITIONALCMD ->if EXPRESSION then COMMANDSEQ else COMMANDSEQ fi 


EXPRESSION ->EXPRESSION minus EXPRESSION 
            | EXPRESSION plus EXPRESSION 
            | EXPRESSION mult EXPRESSION
            | EXPRESSION divide EXPRESSION 
            | EXPRESSION ratplus EXPRESSION 
            | EXPRESSION ratminus EXPRESSION 
            | EXPRESSION ratmult EXPRESSION 
            | EXPRESSION ratdivide EXPRESSION 
            | EXPRESSION mod EXPRESSION 
            | EXPRESSION andalso EXPRESSION 
            | EXPRESSION orelse EXPRESSION 
            | EXPRESSION equal EXPRESSION
            | EXPRESSION notequal EXPRESSION 
            | EXPRESSION lessthan EXPRESSION 
            | EXPRESSION lessthanequal EXPRESSION 
            | EXPRESSION morethan EXPRESSION 
            | EXPRESSION morethanequal EXPRESSION 
            | lparen EXPRESSION rparen  
            | not EXPRESSION  | unminus EXPRESSION 
            | ratinverse EXPRESSION 
             | IDENT  | INTEGER 
            | fromdecimal lparen RATIONAL rparen 
            | tt | ff 
            | makerat lparen EXPRESSION comma EXPRESSION rparen



