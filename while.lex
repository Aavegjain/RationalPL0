structure T = Tokens 

type pos = int ; 
type svalue = T.svalue 
type ('a,'b) token = ('a,'b) T.token
type lexresult = (svalue,pos) token 
type lexarg = string 
type arg = lexarg 

val lin = ref 1;
val col = ref 0;
val eolpos = ref 0; 

val badCh : string * string * int * int -> unit = fn
(fileName,bad,line,col) =>
TextIO.output(TextIO.stdOut,fileName^"["^Int.toString line^"."^Int.toString col
^"] Invalid character \""^bad^"\"\n");
val eof = fn fileName => T.EOF (!lin,!col);

%%
%full
%header (functor WhileLexFun(structure Tokens: While_TOKENS));
%arg (fileName:string);
%s PI COMMENT;

alpha=[A-Za-z];
digit=[0-9];
sign = [+|~] ;
ws = [\ \t];

%%
<INITIAL>{ws}* => (lin:=1; eolpos:=0; YYBEGIN PI; continue ());  
<PI>{ws}* => ( continue() ) ; 
<PI>"\n" => (lin := (!lin) + 1 ; eolpos := yypos + size yytext ; continue()  ) ;
<PI>{alpha}([A-Za-z0-9]*) => (case yytext of
                            "rational" => (col:=yypos-(!eolpos);
                                        T.RATIONALDECL(!lin,!col))
                            | "integer" => (col:=yypos-(!eolpos);
                                    T.INTEGERDECL(!lin,!col))
                            | "boolean" => (col:=yypos-(!eolpos);
                                    T.BOOLEANDECL(!lin,!col))
                            | "tt" => (col:=yypos-(!eolpos);
                                    T.TT(DataTypes.tt,!lin,!col))
                            | "ff" => (col:=yypos-(!eolpos);
                                    T.FF(DataTypes.ff,!lin,!col))
                            | "var" => (col:=yypos-(!eolpos);
                                    T.VAR(!lin,!col))
                            | "print" => (col:=yypos-(!eolpos);
                                    T.PRINT(!lin,!col))
                            | "call" => (col:=yypos-(!eolpos);
                                    T.CALL(!lin,!col))
                            | "read" => (col:=yypos-(!eolpos);
                                    T.READ(!lin,!col))
                            | "procedure" => (col:=yypos-(!eolpos);
                                    T.PROCEDURE(!lin,!col))
                            | "inverse" => (col:=yypos-(!eolpos);
                                    T.RATINVERSE(!lin,!col))
                            | "while" => (col := yypos - (!eolpos) ; 
                                    T.WHILE(!lin, !col)) 
                            | "do" => (col := yypos - (!eolpos) ; 
                                    T.DO(!lin, !col))
                            | "od" => (col := yypos - (!eolpos) ; 
                                    T.OD(!lin, !col))
                            | "if" => (col := yypos - (!eolpos) ; 
                                    T.IF(!lin, !col)) 
                            | "fi" => (col := yypos - (!eolpos) ; 
                                    T.FI(!lin, !col)) 
                            | "then" => (col := yypos - (!eolpos) ; 
                                    T.THEN(!lin, !col)) 
                            | "else" => (col := yypos - (!eolpos) ; 
                                    T.ELSE(!lin, !col))
                            
                            | "showRat" =>  (col := yypos - (!eolpos) ; 
                                    T.SHOWRAT(!lin, !col))
                            | "showDecimal" =>  (col := yypos - (!eolpos) ; 
                                    T.SHOWDECIMAL(!lin, !col))
                            | "fromDecimal" =>  (col := yypos - (!eolpos) ; 
                                    T.FROMDECIMAL(!lin, !col))
                            | "toDecimal" =>  (col := yypos - (!eolpos) ; 
                                    T.TODECIMAL(!lin, !col))
                            |  "rat" => (col := yypos - (!eolpos) ; 
                                    T.RAT(!lin, !col))
                            | _ => (col:=yypos-(!eolpos);
                                    T.IDENT(yytext,!lin,!col)) );
<PI>"make_rat" =>  (col := yypos - (!eolpos) ; T.MAKERAT(!lin, !col));
<PI>{sign}?{digit}+ => (col:=yypos-(!eolpos); Tokens.INTEGER( (Bigint.make_from_str(yytext)),!lin,!col)) ;
<PI>{sign}?({digit}*)"."({digit}*)"("({digit}+)")"  => (col:=yypos-(!eolpos); Tokens.RATIONAL( (Rational.fromDecimal(yytext)),!lin,!col)) ; 
<PI>"-" => (col:=yypos-(!eolpos); T.MINUS(!lin,!col));
<PI>"+" => (col:=yypos-(!eolpos); T.PLUS(!lin,!col));
<PI>"*" => (col:=yypos-(!eolpos); T.MULT(!lin,!col));
<PI>"/" => (col:=yypos-(!eolpos); T.DIVIDE(!lin,!col));
<PI>"%" => (col:=yypos-(!eolpos); T.MOD(!lin,!col));
<PI>"~" =>  (col:=yypos-(!eolpos); T.UNMINUS(!lin,!col));
<PI>".-." => (col:=yypos-(!eolpos); T.RATMINUS(!lin,!col));
<PI>".+." => (col:=yypos-(!eolpos); T.RATPLUS(!lin,!col));
<PI>".*." => (col:=yypos-(!eolpos); T.RATMULT(!lin,!col));
<PI>"./." => (col:=yypos-(!eolpos); T.RATDIVIDE(!lin,!col));

<PI>"!" => (col:=yypos-(!eolpos); T.NOT(!lin,!col));
<PI>"&&" => (col:=yypos-(!eolpos); T.ANDALSO(!lin,!col));
<PI>"||" => (col:=yypos-(!eolpos); T.ORELSE(!lin,!col));
<PI>"=" => (col:=yypos-(!eolpos); T.EQUAL(!lin,!col));
<PI>"<>" => (col:=yypos-(!eolpos); T.NOTEQUAL(!lin,!col));
<PI>">" => (col:=yypos-(!eolpos); T.MORETHAN(!lin,!col));
<PI>">=" => (col:=yypos-(!eolpos); T.MORETHANEQUAL(!lin,!col));
<PI>"<" => (col:=yypos-(!eolpos); T.LESSTHAN(!lin,!col));
<PI>"<=" => (col:=yypos-(!eolpos); T.LESSTHANEQUAL(!lin,!col)); 

<PI>":=" => (col:=yypos-(!eolpos); T.ASSIGN(!lin,!col));
<PI>"(" => (col:=yypos-(!eolpos); T.LPAREN(!lin,!col));
<PI>")" => (col:=yypos-(!eolpos); T.RPAREN(!lin,!col));
<PI>"{" => (col:=yypos-(!eolpos); T.CURLYLPAREN(!lin,!col));
<PI>"}" => (col:=yypos-(!eolpos); T.CURLYRPAREN(!lin,!col)) ;
<PI>";" => (col:=yypos-(!eolpos); T.SEMICOLON(!lin,!col));
<PI>"," => (col:=yypos-(!eolpos); T.COMMA(!lin,!col));
<PI>"(*" => (YYBEGIN COMMENT; continue ());
<PI>. => (col:=yypos-(!eolpos);badCh (fileName,yytext,!lin,!col); T.ILLCH(!lin,!col));

<COMMENT>"\n" => (lin:=(!lin)+1;eolpos:=yypos+size yytext; continue()  ) ;
<COMMENT>"*)" => (YYBEGIN PI; continue ()) ;
<COMMENT>. => (continue ());

  






