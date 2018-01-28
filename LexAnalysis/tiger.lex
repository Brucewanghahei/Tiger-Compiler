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
<INITIAL> var => (Tokens.VAR(yypos, yypos+3));
<INITIAL> "break" => (Tokens.BREAK(yypos, yypos+5));
<INITIAL> "of" => (Tokens.OF(yypos, yypos+2));
<INITIAL> "end" => (Tokens.END(yypos, yypos+3));
<INITIAL> "let" => (Tokens.LET(yypos, yypos+3));
<INITIAL> "array" => (Tokens.ARRAY(yypos, yypos+5));
<INITIAL> ":=" => (Tokens.ASSIGN(yypos, yypos+2));
<INITIAL> "=" => (Tokens.ASSIGN(yypos, yypos+1));
<INITIAL> "}" => (Tokens.RBRACE(yypos, yypos+1));
<INITIAL> "{" => (Tokens.LBRACE(yypos, yypos+1));
<INITIAL> "]" => (Tokens.RBRACK(yypos, yypos+1));
<INITIAL> "[" => (Tokens.LBRACK(yypos, yypos+1));
<INITIAL> ")" => (Tokens.RPAREN(yypos, yypos+1));
<INITIAL> "(" => (Tokens.LPAREN(yypos, yypos+1));
<INITIAL> ";" => (Tokens.SEMICOLON(yypos, yypos+1));
<INITIAL> ":" => (Tokens.COLON(yypos, yypos+1));
<INITIAL> ","	=> (Tokens.COMMA(yypos, yypos+1));
<INITIAL> in => (Tokens.IN(yypos, yypos + 2));
<INITIAL> nil => (Tokens.NIL(yypos, yypos + 3));
<INITIAL> function => (Tokens.FUNCTION(yypos, yypos + 8));
<INITIAL> var => (Tokens.VAR(yypos, yypos + 3));
<INITIAL> do => (Tokens.DO(yypos, yypos + 2));
<INITIAL> to => (Tokens.TO(yypos, yypos + 2));
<INITIAL> for => (Tokens.FOR(yypos, yypos + 3));
<INITIAL> while => (Tokens.WHILE(yypos, yypos + 5));
<INITIAL> else => (Tokens.ELSE(yypos, yypos + 4));
<INITIAL> then => (Tokens.THEN(yypos, yypos + 4));
<INITIAL> if => (Tokens.IF(yypos, yypos + 2));
bbb => (Tokens.ID("\"", yypos, yypos+1));
\"([^\\"]|\\([nt"]|\^.|[0-9]{3}|[\t\n\ ]*\\))*\" => (let val text = String.translate(fn ch =>
						     case ch of
										 (#"\n" | #"\t" | #" ") => "" 
										 |  _ => String.str(ch)
						     ) yytext;
						     in Tokens.STRING(text, yypos, yypos + 2) end);
. => (err (yypos, ("illegal character " ^ yytext)); continue());

