signature While_TOKENS =
sig
type ('a,'b) token
type svalue
val UNMINUS:  'a * 'a -> (svalue,'a) token
val FROMDECIMAL:  'a * 'a -> (svalue,'a) token
val TODECIMAL:  'a * 'a -> (svalue,'a) token
val SHOWRAT:  'a * 'a -> (svalue,'a) token
val SHOWDECIMAL:  'a * 'a -> (svalue,'a) token
val RAT:  'a * 'a -> (svalue,'a) token
val MAKERAT:  'a * 'a -> (svalue,'a) token
val EOF:  'a * 'a -> (svalue,'a) token
val ILLCH:  'a * 'a -> (svalue,'a) token
val COMMA:  'a * 'a -> (svalue,'a) token
val SEMICOLON:  'a * 'a -> (svalue,'a) token
val CURLYRPAREN:  'a * 'a -> (svalue,'a) token
val CURLYLPAREN:  'a * 'a -> (svalue,'a) token
val RPAREN:  'a * 'a -> (svalue,'a) token
val LPAREN:  'a * 'a -> (svalue,'a) token
val CALL:  'a * 'a -> (svalue,'a) token
val PROCEDURE:  'a * 'a -> (svalue,'a) token
val FI:  'a * 'a -> (svalue,'a) token
val ELSE:  'a * 'a -> (svalue,'a) token
val THEN:  'a * 'a -> (svalue,'a) token
val IF:  'a * 'a -> (svalue,'a) token
val OD:  'a * 'a -> (svalue,'a) token
val DO:  'a * 'a -> (svalue,'a) token
val WHILE:  'a * 'a -> (svalue,'a) token
val ASSIGN:  'a * 'a -> (svalue,'a) token
val MORETHANEQUAL:  'a * 'a -> (svalue,'a) token
val MORETHAN:  'a * 'a -> (svalue,'a) token
val LESSTHANEQUAL:  'a * 'a -> (svalue,'a) token
val LESSTHAN:  'a * 'a -> (svalue,'a) token
val NOTEQUAL:  'a * 'a -> (svalue,'a) token
val EQUAL:  'a * 'a -> (svalue,'a) token
val RATINVERSE:  'a * 'a -> (svalue,'a) token
val ORELSE:  'a * 'a -> (svalue,'a) token
val ANDALSO:  'a * 'a -> (svalue,'a) token
val NOT:  'a * 'a -> (svalue,'a) token
val RATPLUS:  'a * 'a -> (svalue,'a) token
val RATMINUS:  'a * 'a -> (svalue,'a) token
val RATMULT:  'a * 'a -> (svalue,'a) token
val RATDIVIDE:  'a * 'a -> (svalue,'a) token
val MOD:  'a * 'a -> (svalue,'a) token
val MULT:  'a * 'a -> (svalue,'a) token
val DIVIDE:  'a * 'a -> (svalue,'a) token
val PLUS:  'a * 'a -> (svalue,'a) token
val MINUS:  'a * 'a -> (svalue,'a) token
val BOOLEAN: (DataTypes.Boolean) *  'a * 'a -> (svalue,'a) token
val RATIONAL: (Rational.rational) *  'a * 'a -> (svalue,'a) token
val INTEGER: (Bigint.bigint) *  'a * 'a -> (svalue,'a) token
val IDENT: (string) *  'a * 'a -> (svalue,'a) token
val INVERSE:  'a * 'a -> (svalue,'a) token
val READ:  'a * 'a -> (svalue,'a) token
val PRINT:  'a * 'a -> (svalue,'a) token
val VAR:  'a * 'a -> (svalue,'a) token
val FF: (DataTypes.Boolean) *  'a * 'a -> (svalue,'a) token
val TT: (DataTypes.Boolean) *  'a * 'a -> (svalue,'a) token
val BOOLEANDECL:  'a * 'a -> (svalue,'a) token
val INTEGERDECL:  'a * 'a -> (svalue,'a) token
val RATIONALDECL:  'a * 'a -> (svalue,'a) token
end
signature While_LRVALS=
sig
structure Tokens : While_TOKENS
structure ParserData:PARSER_DATA
sharing type ParserData.Token.token = Tokens.token
sharing type ParserData.svalue = Tokens.svalue
end