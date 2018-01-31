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
val stringFlag = ref false
fun stringEmpBuffer curP = let val () = stringBuffer := [""] val () = stringBegin := curP val () = stringFlag := true in () end
fun stringAppBuffer (str:string) = stringBuffer := str :: !stringBuffer
fun stringBldBuffer () = let val () = stringFlag := false in String.concat(List.revAppend(!stringBuffer, [""])) end

fun eof() =
    let
	val pos = hd(!linePos)
	val () = if !commentLevel <> 0 then
	    err(pos, "unclosed comment")
	else if !stringFlag = true then
	    err(pos, "unclosed string")
	else
            ();
    in
        Tokens.EOF(pos,pos)
    end

%% 
%s COMMENT STRING STRESCAPE;
newline = \n;
whitespace = [\t\ ]+;
letter = [A-Za-z];
digit = [0-9];
%%
<INITIAL> {whitespace} => (continue());
<INITIAL, COMMMENT> {newline}	=> (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
<INITIAL> "/*" => (YYBEGIN COMMENT; commentIncLevel(); continue());
<COMMENT> "/*" => (commentIncLevel(); continue());
<COMMENT> "*/" => (commentDecLevel(); if !commentLevel = 0 then YYBEGIN INITIAL else (); continue());
<COMMENT> . => (continue());
<INITIAL> ":=" => (Tokens.ASSIGN(yypos, yypos + size yytext));
<INITIAL> "}" => (Tokens.RBRACE(yypos, yypos + size yytext));
<INITIAL> "{" => (Tokens.LBRACE(yypos, yypos + size yytext));
<INITIAL> "]" => (Tokens.RBRACK(yypos, yypos + size yytext));
<INITIAL> "[" => (Tokens.LBRACK(yypos, yypos + size yytext));
<INITIAL> ")" => (Tokens.RPAREN(yypos, yypos + size yytext));
<INITIAL> "(" => (Tokens.LPAREN(yypos, yypos + size yytext));
<INITIAL> ";" => (Tokens.SEMICOLON(yypos, yypos + size yytext));
<INITIAL> ":" => (Tokens.COLON(yypos, yypos + size yytext));
<INITIAL> ","	=> (Tokens.COMMA(yypos, yypos + size yytext));
<INITIAL> "|" => (Tokens.OR(yypos, yypos + size yytext)) ;
<INITIAL> "&" => (Tokens.AND(yypos, yypos + size yytext));
<INITIAL> "<>" => (Tokens.NEQ(yypos, yypos + size yytext));
<INITIAL> ">=" => (Tokens.GE(yypos, yypos + size yytext));
<INITIAL> "<=" => (Tokens.LE(yypos, yypos + size yytext));
<INITIAL> ">" => (Tokens.GT(yypos, yypos + size yytext));
<INITIAL> "<" => (Tokens.LT(yypos, yypos + size yytext));
<INITIAL> "=" => (Tokens.EQ(yypos, yypos + size yytext));
<INITIAL> "/" => (Tokens.DIVIDE(yypos, yypos + size yytext));
<INITIAL> "*" => (Tokens.TIMES(yypos, yypos + size yytext));
<INITIAL> "-" => (Tokens.MINUS(yypos, yypos + size yytext));
<INITIAL> "+" => (Tokens.PLUS(yypos, yypos + size yytext));
<INITIAL> "." => (Tokens.DOT(yypos, yypos + size yytext));
<INITIAL> var => (Tokens.VAR(yypos, yypos + size yytext));
<INITIAL> break => (Tokens.BREAK(yypos, yypos + size yytext));
<INITIAL> of => (Tokens.OF(yypos, yypos + size yytext));
<INITIAL> end => (Tokens.END(yypos, yypos + size yytext));
<INITIAL> let => (Tokens.LET(yypos, yypos + size yytext));
<INITIAL> array => (Tokens.ARRAY(yypos, yypos + size yytext));
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
<INITIAL> type => (Tokens.TYPE(yypos, yypos + size yytext));
<INITIAL> {digit}+ => (Tokens.INT(valOf(Int.fromString(yytext)), yypos, yypos + size(yytext)));
<INITIAL> [\"] => (YYBEGIN STRING; stringEmpBuffer(yypos); continue());
<STRING> \\ => (YYBEGIN STRESCAPE; continue());
<STRING> [\"] => (YYBEGIN INITIAL; Tokens.STRING(stringBldBuffer(), !stringBegin, yypos+1));
<STRING> [^\n\r] => (stringAppBuffer(yytext); continue());
<STRESCAPE> n|t|"^"[A-Za-z]|[0-9]{3}|\\|[\"] => (YYBEGIN STRING; stringAppBuffer("\\" ^ yytext); continue());
<STRESCAPE> [\000-\037]+\\ => (YYBEGIN STRING; continue());
<INITIAL> {letter}({letter}|{digit}|_)* => (Tokens.ID(yytext, yypos, yypos + size yytext));
. => (YYBEGIN INITIAL; err (yypos, ("illegal character " ^ yytext)); continue());
