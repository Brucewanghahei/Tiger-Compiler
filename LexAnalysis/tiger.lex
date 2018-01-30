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
%s COMMENT;
newline = \n;
space = [\ \t\r]
%%
<INITIAL> {space}+ => (continue());
<INITIAL, COMMEMT> {newline} => (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
<INITIAL> "/*" => (YYBEGIN COMMENT; commentIncLevel(); continue());
<COMMENT> "/*" => (commentIncLevel(); continue());
<COMMENT> "*/" => (commentDecLevel(); if !commentLevel = 0 then YYBEGIN INITIAL else (); continue());
<COMMENT> . => (continue());
<INITIAL> var => (Tokens.VAR(yypos, yypos + size yytext));
<INITIAL> break => (Tokens.BREAK(yypos, yypos + size yytext));
<INITIAL> of => (Tokens.OF(yypos, yypos + size yytext));
<INITIAL> end => (Tokens.END(yypos, yypos + size yytext));
<INITIAL> let => (Tokens.LET(yypos, yypos + size yytext));
<INITIAL> array => (Tokens.ARRAY(yypos, yypos + size yytext));
<INITIAL> ":=" => (Tokens.ASSIGN(yypos, yypos + size yytext));
<INITIAL> "=" => (Tokens.ASSIGN(yypos, yypos + size yytext));
<INITIAL> "}" => (Tokens.RBRACE(yypos, yypos + size yytext));
<INITIAL> "{" => (Tokens.LBRACE(yypos, yypos + size yytext));
<INITIAL> "]" => (Tokens.RBRACK(yypos, yypos + size yytext));
<INITIAL> "[" => (Tokens.LBRACK(yypos, yypos + size yytext));
<INITIAL> ")" => (Tokens.RPAREN(yypos, yypos + size yytext));
<INITIAL> "(" => (Tokens.LPAREN(yypos, yypos + size yytext));
<INITIAL> ";" => (Tokens.SEMICOLON(yypos, yypos + size yytext));
<INITIAL> ":" => (Tokens.COLON(yypos, yypos + size yytext));
<INITIAL> ","	=> (Tokens.COMMA(yypos, yypos + size yytext));
<INITIAL> in => (Tokens.IN(yypos, yypos + size yytext));
<INITIAL> nil => (Tokens.NIL(yypos, yypos + size yytext));
<INITIAL> function => (Tokens.FUNCTION(yypos, yypos + size yytext));
<INITIAL> var => (Tokens.VAR(yypos, yypos + size yytext));
<INITIAL> do => (Tokens.DO(yypos, yypos + size yytext));
<INITIAL> to => (Tokens.TO(yypos, yypos + size yytext));
<INITIAL> for => (Tokens.FOR(yypos, yypos + size yytext));
<INITIAL> while => (Tokens.WHILE(yypos, yypos + size yytext));
<INITIAL> else => (Tokens.ELSE(yypos, yypos + size yytext));
<INITIAL> then => (Tokens.THEN(yypos, yypos + size yytext));
<INITIAL> if => (Tokens.IF(yypos, yypos + size yytext));
<INITIAL> "|" => (Tokens.OR(yypos, yypos + size yytext)) ;
<INITIAL> "&" => (Tokens.AND(yypos, yypos + size yytext));
<INITIAL> ">=" => (Tokens.GE(yypos, yypos + size yytext));
<INITIAL> ">" => (Tokens.GT(yypos, yypos + size yytext));
<INITIAL> "<=" => (Tokens.LE(yypos, yypos + size yytext));
<INITIAL> "<" => (Tokens.LT(yypos, yypos + size yytext));
<INITIAL> "<>" => (Tokens.NEQ(yypos, yypos + size yytext));
<INITIAL> "=" => (Tokens.EQ(yypos, yypos + size yytext));
<INITIAL> "/" => (Tokens.DIVIDE(yypos, yypos + size yytext));
<INITIAL> "*" => (Tokens.TIMES(yypos, yypos + size yytext));
<INITIAL> "-" => (Tokens.MINUS(yypos, yypos + size yytext));
<INITIAL> "+" => (Tokens.PLUS(yypos, yypos + size yytext));
<INITIAL> "." => (Tokens.DOT(yypos, yypos + size yytext));
<INITIAL> type => (Tokens.TYPE(yypos, yypos + size yytext));
\"([^\\"]|\\([nt"]|\^.|[0-9]{3}|[\t\n\ ]*\\))*\" => (let val text = String.translate(fn ch =>
						     case ch of
										 (#"\n" | #"\t" | #" ") => "" 
										 |  _ => String.str(ch)
						     ) yytext;
						     in Tokens.STRING(text, yypos, yypos + 2) end);
. => (err (yypos, ("illegal character " ^ yytext)); continue());

