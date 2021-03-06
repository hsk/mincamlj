/*
字句解析(lexer.mll)
 
コンピュータにとっては、MLプログラムといえども、はじめはただの文字列です。たとえばさっきのgcdだったら、
「l」「e」「t」「 」「r」「e」「c」「 」「g」「c」「d」「 」「m」「 」「n」「 」「=」...
のように見えるわけです。このままでは何もできないので、まず
「let」「rec」「gcd」「m」「n」「=」...
のような字句に区切ります。この処理を字句解析といいます。
 
字句解析にはいろいろな方法がありますが、ここではocamllexという、まさにOCamlで字句解析をするためのツールを利用します。そのファイルがlexer.mllです。ocamllexについての詳細はマニュアル（ないし和訳）を参照してもらうことにして、概要だけ説明すると、
 
| '-'? digit+
    { INT(int_of_string (Lexing.lexeme lexbuf)) }
 
といったパターンマッチングのような構文により、「正規表現'-'? digit+にマッチしたら字句INTを返す」等のルールを並べて書けばOKです。字句を表すデータ型（INTなど）は、次に述べるparser.mlyで定義されています。Lexing.lexeme lexbufという部分は、解析中の文字列を表す「おまじない」だと思ってください。
 
*/
package mincaml;
import java.io.*;
import mincaml.*;
%%

%byaccj

%{
  private Parser yyparser;

  public Yylex(java.io.Reader r, Parser yyparser) {
    this(r);
    this.yyparser = yyparser;
  }

  public static void main(String[] args) {
    try {
    Parser yyparser = new Parser(new FileReader(args[0]));
    while(true) {
      int rc = yyparser.yylex();
      System.out.println(rc);
      if (rc <= 0) break;
      if (rc == Parser.EOF) break;
    }
    } catch (Exception e){
      e.printStackTrace();
    }
  }
%}

space = [ \t\n\r]
digit = [0-9]
lower = [a-z]
upper = [A-Z]
let = let
%%

{space}+ {} // { token lexbuf } skip
"(*" {} // { comment lexbuf; token lexbuf } // ネストしたコメントのためのトリック
"(" { return Parser.LPAREN; }
")" { return Parser.RPAREN; }
"true"  { yyparser.yylval = new ParserVal(new Boolean(true)); return Parser.BOOL; }
"false" { yyparser.yylval = new ParserVal(new Boolean(false)); return Parser.BOOL; }
"not"   { return Parser.NOT; }
{digit}+ // 整数を字句解析するルール (caml2html: lexer_int)
    { yyparser.yylval = new ParserVal(Integer.parseInt(yytext())); return Parser.INT; }
{digit}+ ("." {digit}*)? ([eE] [+-]? {digit}+)?
    { yyparser.yylval = new ParserVal(Double.parseDouble(yytext())); return Parser.FLOAT; }
"-" // -.より後回しにしなくても良い? 最長一致?
    { return Parser.MINUS; }
"+" // +.より後回しにしなくても良い? 最長一致?
    { return Parser.PLUS; }
"-."
    { return Parser.MINUS_DOT; }
"+."
    { return Parser.PLUS_DOT; }
"*."
    { return Parser.AST_DOT; }
"/."
    { return Parser.SLASH_DOT; }
"="
    { return Parser.EQUAL; }
"<>"
    { return Parser.LESS_GREATER; }
"<="
    { return Parser.LESS_EQUAL; }
">="
    { return Parser.GREATER_EQUAL; }
"<"
    { return Parser.LESS; }
">"
    { return Parser.GREATER; }
"if"
    { return Parser.IF; }
"then"
    { return Parser.THEN; }
"else"
    { return Parser.ELSE; }
"let"
    { return Parser.LET; }


"in"
    { return Parser.IN; }
"rec"
    { return Parser.REC; }
","
    { return Parser.COMMA; }
"_"
    { yyparser.yylval = new ParserVal((Object)Id.gentmp(new Type.Unit())); return Parser.IDENT; }
"Array.create" // [XX] ad hoc
    { return Parser.ARRAY_CREATE; }
"."
    { return Parser.DOT; }
"<-"
    { return Parser.LESS_MINUS; }
";"
    { return Parser.SEMICOLON; }
<<EOF>>
    { /*return Parser.EOF;*/ return YYEOF;}
{lower} ({digit}|{lower}|{upper}|"_")* // 他の「予約語」より後でないといけない *)
    { yyparser.yylval = new ParserVal(""+yytext()); return Parser.IDENT;  }


/*
| _
    { failwith
	(Printf.sprintf "unknown token %s near characters %d-%d"
	   (Lexing.lexeme lexbuf)
	   (Lexing.lexeme_start lexbuf)
	   (Lexing.lexeme_end lexbuf)) }
and comment = parse
| "*)"
    { () }
| "(*"
    { comment lexbuf;
      comment lexbuf }
| eof
    { Format.eprintf "warning: unterminated comment@." }
| _
    { comment lexbuf }
*/

.      {  // token desconocido: se produce un mensaje de error 
          yyparser.yyerror("el(los) caracter(es) '"+yytext()+"' no forma(n) ningun token conocido"); 
       }
