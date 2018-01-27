type pos = int
type lexresult = Tokens.token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
fun err(pos, msg) = ErrorMsg.error pos msg

fun eof() = let val pos = hd(!linePos) in Tokens.EOF(pos,pos) end

%% 
%s INVALID COMMENT COLON FUNCTION TYPE;
%%
<INVALID> . => (err (yypos, "invalid state"); continue());
<COMMENT> . => (continue());
\n	=> (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
var => (Tokens.VAR(yypos, yypos+3));
"}" => (Tokens.RBRACE(yypos, yypos+1));
"{" => (Tokens.LBRACE(yypos, yypos+1));
"]" => (Tokens.RBRACK(yypos, yypos+1));
"[" => (Tokens.LBRACK(yypos, yypos+1));
")" => (Tokens.RPAREN(yypos, yypos+1));
"(" => (Tokens.LPAREN(yypos, yypos+1));
";" => (Tokens.SEMICOLON(yypos, yypos+1));
":" => (YYBEGIN COLON; Tokens.COLON(yypos, yypos+1));
","	=> (Tokens.COMMA(yypos, yypos+1));
"let" => (Tokens.LET(yypos, yypos+3));
"break" => (Tokens.BREAK(yypos, yypos+5));
"of" => (Tokens.OF(yypos, yypos+2));
"end" => (Tokens.END(yypos, yypos+3));
"array" => (Tokens.ARRAY(yypos, yypos+5));
<COLON, FUNCTION, TYPE> "=" => (YYBEGIN INITIAL; Tokens.ASSIGN(yypos, yypos+1));
. => (err (yypos, ("illegal character " ^ yytext)); continue());

