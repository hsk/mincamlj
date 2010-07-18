%{
  import java.io.*;
%}

/* 字句を表すデータ型の定義 (caml2html: parser_token) */
%token <obj> BOOL
%token <ival> INT
%token <dval> FLOAT
%token NOT
%token MINUS
%token PLUS
%token MINUS_DOT
%token PLUS_DOT
%token AST_DOT
%token SLASH_DOT
%token EQUAL
%token LESS_GREATER
%token LESS_EQUAL
%token GREATER_EQUAL
%token LESS
%token GREATER
%token IF
%token THEN
%token ELSE
%token <sval> IDENT
%token LET
%token IN
%token REC
%token COMMA
%token ARRAY_CREATE
%token DOT
%token LESS_MINUS
%token SEMICOLON
%token LPAREN
%token RPAREN
%token EOF

/* 優先順位とassociativityの定義（低い方から高い方へ） (caml2html: parser_prior) */
%right prec_let
%right SEMICOLON
%right prec_if
%right LESS_MINUS
%left COMMA
%left EQUAL LESS_GREATER LESS GREATER LESS_EQUAL GREATER_EQUAL
%left PLUS MINUS PLUS_DOT MINUS_DOT
%left AST_DOT SLASH_DOT
%right prec_unary_minus
%left prec_app
%left DOT

/* 開始記号の定義 */
%type <obj> exp simple_exp fundef actual_args elems pat
%type <obj> formal_args
%start exp

%%

simple_exp /* 括弧をつけなくても関数の引数になれる式 (caml2html: parser_simple) */
: LPAREN exp RPAREN
    { $$ = $2; }
| LPAREN RPAREN
    { $$ = new Unit(); }
| BOOL
    { $$ = new Bool((Boolean)$1); }
| INT
    { $$ = new Int($1); }
| FLOAT
    { $$ = new Syntax.Float($1); }
| IDENT { $$ = new Var((String)$1); }
| simple_exp DOT LPAREN exp RPAREN
    { $$ = new Get((T)$1, (T)$4); }

exp /* 一般の式 (caml2html: parser_exp) */
: simple_exp
    { $$ = $1; }
| NOT exp
    %prec prec_app
    { $$ = new Not((T)$2); }
| MINUS exp
    %prec prec_unary_minus
    {
        if ($2 instanceof Syntax.Float) {
            $$ = new Syntax.Float(-((Syntax.Float)$2).a());
            // -1.23などは型エラーではないので別扱い
        } else {
            $$ = new Neg((T)$2);
        }
    }
| exp PLUS exp /* 足し算を構文解析するルール (caml2html: parser_add) */
    { $$ = new Add((T)$1, (T)$3); }
| exp MINUS exp
    { $$ = new Sub((T)$1, (T)$3); }
| exp EQUAL exp
    { $$ = new Eq((T)$1, (T)$3); }
| exp LESS_GREATER exp
    { $$ = new Not(new Eq((T)$1, (T)$3)); }
| exp LESS exp
    { $$ = new Not(new LE((T)$3, (T)$1)); }
| exp GREATER exp
    { $$ = new Not(new LE((T)$1, (T)$3)); }
| exp LESS_EQUAL exp
    { $$ = new LE((T)$1, (T)$3); }
| exp GREATER_EQUAL exp
    { $$ = new LE((T)$3, (T)$1); }
| IF exp THEN exp ELSE exp
    %prec prec_if
    { $$ = new If((T)$2, (T)$4, (T)$6); }
| MINUS_DOT exp
    %prec prec_unary_minus
    { $$ = new FNeg((T)$2); }
| exp PLUS_DOT exp
    { $$ = new FAdd((T)$1, (T)$3); }
| exp MINUS_DOT exp
    { $$ = new FSub((T)$1, (T)$3); }
| exp AST_DOT exp
    { $$ = new FMul((T)$1, (T)$3); }
| exp SLASH_DOT exp
    { $$ = new FDiv((T)$1, (T)$3); }
| LET IDENT EQUAL exp IN exp
    %prec prec_let
    { $$ = let(addtyp((String)$2), (T)$4, (T)$6); }
| LET REC fundef IN exp
    %prec prec_let
    { $$ = new LetRec((Fundef)$3, (T)$5); }
| exp actual_args
    %prec prec_app
    { $$ = app((T)$1, (scala.List<T>)$2); }
| elems
    { System.out.println($1); $$ = tuple((scala.List<T>)$1); }
| LET LPAREN pat RPAREN EQUAL exp IN exp
    { $$ = letTuple((scala.List<scala.Tuple2<String,Type.T>>)$3, (T)$6, (T)$8); }
| simple_exp DOT LPAREN exp RPAREN LESS_MINUS exp
    { $$ = new Put((T)$1, (T)$4, (T)$7); }
| exp SEMICOLON exp
    { $$ = let(tuple2(Id.gentmp(new Type.Unit()), new Type.Unit()), (T)$1, (T)$3); }
| ARRAY_CREATE simple_exp simple_exp
    %prec prec_app
    { $$ = new Array((T)$2, (T)$3); }
/*| error
    { failwith
	(Printf.sprintf "parse error near characters %d-%d"
	   (Parsing.symbol_start ())
	   (Parsing.symbol_end ())) }
*/

fundef
: IDENT formal_args EQUAL exp
    { $$ = fundef(addtyp((String)$1), (scala.List<scala.Tuple2<String,Type.T>>)$2, (T)$4); }

formal_args
: IDENT formal_args
    { $$ = addList2(addtyp((String)$1),(scala.List<scala.Tuple2<String,Type.T>>)$2); }
| IDENT
    { $$ = list2(addtyp((String)$1)); }

actual_args
: actual_args simple_exp
    %prec prec_app
    { $$ = concatList((scala.List<T>)$1, list((T)$2)); }
| simple_exp
    %prec prec_app
    { $$ = list((T)$1); }

elems
: elems COMMA exp
    { $$ = concatList((scala.List<T>)$1, list((T)$3)); }
| exp COMMA exp
    { $$ = addList((T)$1,list((T)$3)); }

pat
: pat COMMA IDENT
    { $$ = concatList2((scala.List<scala.Tuple2<String,Type.T>>)$1, list2(addtyp((String)$3))); }
| IDENT COMMA IDENT
    { $$ = addList2(addtyp((String)$1),list2(addtyp((String)$3))); }

%%
  public Yylex lexer;

  public int yylex () {
    int yyl_return = -1;
    try {
      yyl_return = lexer.yylex();
    }
    catch (IOException e) {
      System.err.println("IO error :"+e);
    }
    return yyl_return;
  }

  public void yyerror (String error) {
    System.err.println ("Error: " + error);
  }

  public Parser(Reader r) {
    lexer = new Yylex(r, this);
  }

  public static void main(String args[]) throws IOException {
    Parser yyparser = new Parser(new FileReader(args[0]));
    System.out.println(yyparser.yyparse());
    System.out.println(yyparser.yyval.obj);
    System.out.println(Typing.f((Syntax.T)yyparser.yyval.obj));
  }
