type pos = int
type lexresult = Tokens.token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
fun err(pos, msg) = ErrorMsg.error pos msg

val commentLevel = ref 0
fun commentIncLevel() = commentLevel := !commentLevel + 1
fun commentDecLevel() = commentLevel := !commentLevel - 1

val stringBuffer = ref [""]
val stringBegin = ref 0
fun stringEmpBuffer curP = let val () = stringBuffer := [""] val () = stringBegin := curP in () end
fun stringAppBuffer (str:string) = stringBuffer := str :: !stringBuffer
fun stringBldBuffer () = String.concat(List.revAppend(!stringBuffer, [""]))

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
%s INVALID COMMENT STRING STRESCAPE;
newline = \n;
whitespace = [\t\ ]+;
%%
<INVALID> . => (err (yypos, "invalid state " ^ yytext); continue());
<INITIAL> {whitespace} => (continue());
<INITIAL, COMMMENT> {newline}	=> (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
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
<INITIAL> "|" => (Tokens.OR(yypos, yypos + 1)) ;
<INITIAL> "&" => (Tokens.AND(yypos, yypos + 1));
<INITIAL> ">=" => (Tokens.GE(yypos, yypos + 2));
<INITIAL> ">" => (Tokens.GT(yypos, yypos + 1));
<INITIAL> "<=" => (Tokens.LE(yypos, yypos + 2));
<INITIAL> "<" => (Tokens.LT(yypos, yypos + 1));
<INITIAL> "<>" => (Tokens.NEQ(yypos, yypos + 2));
<INITIAL> "=" => (Tokens.EQ(yypos, yypos + 1));
<INITIAL> "/" => (Tokens.DIVIDE(yypos, yypos + 1));
<INITIAL> "*" => (Tokens.TIMES(yypos, yypos + 1));
<INITIAL> "-" => (Tokens.MINUS(yypos, yypos + 1));
<INITIAL> "+" => (Tokens.PLUS(yypos, yypos + 1));
<INITIAL> "." => (Tokens.DOT(yypos, yypos + 1));
<INITIAL> "type" => (Tokens.TYPE(yypos, yypos + 4));
<INITIAL> [\"] => (YYBEGIN STRING; stringEmpBuffer(yypos); continue());
<STRING> \\ => (YYBEGIN STRESCAPE; continue());
<STRING> [\"] => (YYBEGIN INITIAL; Tokens.STRING(stringBldBuffer(), !stringBegin, yypos+1));
<STRING> [^\n\r] => (stringAppBuffer(yytext); continue());
<STRESCAPE> n|t|"^"[A-Za-z]|[0-9]{3}|\\|[\"] => (YYBEGIN STRING; stringAppBuffer("\\" ^ yytext); continue());
<STRESCAPE> [\000-\037]+\\ => (YYBEGIN STRING; continue());
. => (YYBEGIN INITIAL; err (yypos, ("illegal character " ^ yytext)); continue());
