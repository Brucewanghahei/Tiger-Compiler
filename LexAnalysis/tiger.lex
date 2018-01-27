type pos = int
type lexresult = Tokens.token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
fun err(pos, msg) = ErrorMsg.error pos msg

val commentLevel = ref 0
fun commentIncLevel() = commentLevel := !commentLevel + 1
fun commentDecLevel() = commentLevel := !commentLevel - 1

fun eof() =
    let
        val pos = hd(!linePos)
    in
        if !commentLevel <> 0 then
            err(pos, "unclosed comment")
        else
            ();
        Tokens.EOF(pos,pos)
    end

%% 
%s INVALID COMMENT;
newline = \n;
%%
<INVALID> . => (err (yypos, "invalid state"); continue());
<INITIAL, COMMEMT> newline	=> (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
<INITIAL> "/*" => (YYBEGIN COMMENT; commentIncLevel(); continue());
<COMMENT> "/*" => (commentIncLevel(); continue());
<COMMENT> "*/" => (commentDecLevel(); if !commentLevel = 0 then YYBEGIN INITIAL else (); continue());
<COMMENT> . => (continue());
var => (Tokens.VAR(yypos, yypos+3));
"break" => (Tokens.BREAK(yypos, yypos+5));
"of" => (Tokens.OF(yypos, yypos+2));
"end" => (Tokens.END(yypos, yypos+3));
"let" => (Tokens.LET(yypos, yypos+3));
"array" => (Tokens.ARRAY(yypos, yypos+5));
":=" => (Tokens.ASSIGN(yypos, yypos+2));
"=" => (Tokens.ASSIGN(yypos, yypos+1));
"}" => (Tokens.RBRACE(yypos, yypos+1));
"{" => (Tokens.LBRACE(yypos, yypos+1));
"]" => (Tokens.RBRACK(yypos, yypos+1));
"[" => (Tokens.LBRACK(yypos, yypos+1));
")" => (Tokens.RPAREN(yypos, yypos+1));
"(" => (Tokens.LPAREN(yypos, yypos+1));
";" => (Tokens.SEMICOLON(yypos, yypos+1));
":" => (Tokens.COLON(yypos, yypos+1));
","	=> (Tokens.COMMA(yypos, yypos+1));
in => (Tokens.IN(yypos, yypos + 2));
nil => (Tokens.NIL(yypos, yypos + 3));
function => (Tokens.FUNCTION(yypos, yypos + 8));
var => (Tokens.VAR(yypos, yypos + 3));
do => (Tokens.DO(yypos, yypos + 2));
to => (Tokens.TO(yypos, yypos + 2));
for => (Tokens.FOR(yypos, yypos + 3));
while => (Tokens.WHILE(yypos, yypos + 5));
else => (Tokens.ELSE(yypos, yypos + 4));
then => (Tokens.THEN(yypos, yypos + 4));
if => (Tokens.IF(yypos, yypos + 2));
bbb => (Tokens.ID("\"", yypos, yypos+1));
\"([^\\"]|\\([nt"]|\^.|[0-9]{3}|[\t\n\ ]*\\))*\" => (let val text = String.translate(fn ch =>
						     case ch of
										 (#"\n" | #"\t" | #" ") => "" 
										 |  _ => String.str(ch)
						     ) yytext;
						     in Tokens.STRING(text, yypos, yypos + 2) end);
. => (err (yypos, ("illegal character " ^ yytext)); continue());

