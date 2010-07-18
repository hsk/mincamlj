//### This file created by BYACC 1.8(/Java extension  1.15)
//### Java capabilities added 7 Jan 97, Bob Jamison
//### Updated : 27 Nov 97  -- Bob Jamison, Joe Nieten
//###           01 Jan 98  -- Bob Jamison -- fixed generic semantic constructor
//###           01 Jun 99  -- Bob Jamison -- added Runnable support
//###           06 Aug 00  -- Bob Jamison -- made state variables class-global
//###           03 Jan 01  -- Bob Jamison -- improved flags, tracing
//###           16 May 01  -- Bob Jamison -- added custom stack sizing
//###           04 Mar 02  -- Yuval Oren  -- improved java performance, added options
//###           14 Mar 02  -- Tomas Hurka -- -d support, static initializer workaround
//### Please send bug reports to tom@hukatronic.cz
//### static char yysccsid[] = "@(#)yaccpar	1.8 (Berkeley) 01/20/90";



package mincaml;



//#line 2 "parser.y"
  import java.io.*;
//#line 19 "Parser.java"




public class Parser
             extends mincaml.Syntax
{

boolean yydebug;        //do I want debug output?
int yynerrs;            //number of errors so far
int yyerrflag;          //was there an error?
int yychar;             //the current working character

//########## MESSAGES ##########
//###############################################################
// method: debug
//###############################################################
void debug(String msg)
{
  if (yydebug)
    System.out.println(msg);
}

//########## STATE STACK ##########
final static int YYSTACKSIZE = 500;  //maximum stack size
int statestk[] = new int[YYSTACKSIZE]; //state stack
int stateptr;
int stateptrmax;                     //highest index of stackptr
int statemax;                        //state when highest index reached
//###############################################################
// methods: state stack push,pop,drop,peek
//###############################################################
final void state_push(int state)
{
  try {
		stateptr++;
		statestk[stateptr]=state;
	 }
	 catch (ArrayIndexOutOfBoundsException e) {
     int oldsize = statestk.length;
     int newsize = oldsize * 2;
     int[] newstack = new int[newsize];
     System.arraycopy(statestk,0,newstack,0,oldsize);
     statestk = newstack;
     statestk[stateptr]=state;
  }
}
final int state_pop()
{
  return statestk[stateptr--];
}
final void state_drop(int cnt)
{
  stateptr -= cnt; 
}
final int state_peek(int relative)
{
  return statestk[stateptr-relative];
}
//###############################################################
// method: init_stacks : allocate and prepare stacks
//###############################################################
final boolean init_stacks()
{
  stateptr = -1;
  val_init();
  return true;
}
//###############################################################
// method: dump_stacks : show n levels of the stacks
//###############################################################
void dump_stacks(int count)
{
int i;
  System.out.println("=index==state====value=     s:"+stateptr+"  v:"+valptr);
  for (i=0;i<count;i++)
    System.out.println(" "+i+"    "+statestk[i]+"      "+valstk[i]);
  System.out.println("======================");
}


//########## SEMANTIC VALUES ##########
//public class ParserVal is defined in ParserVal.java


String   yytext;//user variable to return contextual strings
ParserVal yyval; //used to return semantic vals from action routines
ParserVal yylval;//the 'lval' (result) I got from yylex()
ParserVal valstk[];
int valptr;
//###############################################################
// methods: value stack push,pop,drop,peek.
//###############################################################
void val_init()
{
  valstk=new ParserVal[YYSTACKSIZE];
  yyval=new ParserVal();
  yylval=new ParserVal();
  valptr=-1;
}
void val_push(ParserVal val)
{
  if (valptr>=YYSTACKSIZE)
    return;
  valstk[++valptr]=val;
}
ParserVal val_pop()
{
  if (valptr<0)
    return new ParserVal();
  return valstk[valptr--];
}
void val_drop(int cnt)
{
int ptr;
  ptr=valptr-cnt;
  if (ptr<0)
    return;
  valptr = ptr;
}
ParserVal val_peek(int relative)
{
int ptr;
  ptr=valptr-relative;
  if (ptr<0)
    return new ParserVal();
  return valstk[ptr];
}
final ParserVal dup_yyval(ParserVal val)
{
  ParserVal dup = new ParserVal();
  dup.ival = val.ival;
  dup.dval = val.dval;
  dup.sval = val.sval;
  dup.obj = val.obj;
  return dup;
}
//#### end semantic value section ####
public final static short BOOL=257;
public final static short INT=258;
public final static short FLOAT=259;
public final static short NOT=260;
public final static short MINUS=261;
public final static short PLUS=262;
public final static short MINUS_DOT=263;
public final static short PLUS_DOT=264;
public final static short AST_DOT=265;
public final static short SLASH_DOT=266;
public final static short EQUAL=267;
public final static short LESS_GREATER=268;
public final static short LESS_EQUAL=269;
public final static short GREATER_EQUAL=270;
public final static short LESS=271;
public final static short GREATER=272;
public final static short IF=273;
public final static short THEN=274;
public final static short ELSE=275;
public final static short IDENT=276;
public final static short LET=277;
public final static short IN=278;
public final static short REC=279;
public final static short COMMA=280;
public final static short ARRAY_CREATE=281;
public final static short DOT=282;
public final static short LESS_MINUS=283;
public final static short SEMICOLON=284;
public final static short LPAREN=285;
public final static short RPAREN=286;
public final static short EOF=287;
public final static short prec_let=288;
public final static short prec_if=289;
public final static short prec_unary_minus=290;
public final static short prec_app=291;
public final static short YYERRCODE=256;
final static short yylhs[] = {                           -1,
    1,    1,    1,    1,    1,    1,    1,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    2,    6,    6,    3,    3,    4,    4,    5,
    5,
};
final static short yylen[] = {                            2,
    3,    2,    1,    1,    1,    1,    5,    1,    2,    2,
    3,    3,    3,    3,    3,    3,    3,    3,    6,    2,
    3,    3,    3,    3,    6,    5,    2,    1,    8,    7,
    3,    3,    4,    2,    1,    2,    1,    3,    3,    3,
    3,
};
final static short yydefred[] = {                         0,
    3,    4,    5,    0,    0,    0,    0,    6,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    2,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    1,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
   34,    0,    0,   41,   40,    0,    0,    0,    0,    0,
    0,    0,    7,    0,    0,    0,    0,
};
final static short yydgoto[] = {                         12,
   13,   46,   40,   14,   48,   72,
};
final static short yysindex[] = {                       949,
    0,    0,    0,  949,  949,  949,  949,    0, -270, -193,
 -204,  875, -282, -275, -193, -193, -193,  759, -260, -268,
 -266, -255,    0,  -53,  949,  949,  949,  949,  949,  949,
  949,  949,  949,  949,  949,  949,  949,  949, -265, -193,
 -262,  949,  949,  949, -257, -258, -256, -264, -259, -265,
    0, -162, -162, -162, -162, -193, -193,  974,  974,  974,
  974,  974,  974,  929,  875, -265,  949,  929,  788,  817,
 -257, -242,  949, -248, -247, -221,  949,  699,  949,  949,
    0,  949,  875,    0,    0,  949,  729, -236,  904,  875,
  875,  846,    0,  949,  949,  904,  875,
};
final static short yyrindex[] = {                         0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,   31,  151,  177,  203,  229,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,   61,  255,
    0,    0,    0,    0,    0,    0,    0,    0,    0,   91,
    0,  334,  364,  394,  424,  282,  308,  450,  476,  502,
  528,  555,  581,  599,  542,  121,    0,  613,    0,    0,
 -219,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,  645,    0,    0,    0,    0,    1,  629,  659,
 -226,    0,    0,    0,    0,  643,  673,
};
final static short yygindex[] = {                         7,
  100,    0,    0,    0,    0,  -13,
};
final static int YYTABLESIZE=1259;
static short yytable[];
static { yytable();}
static void yytable(){
yytable = new short[]{                         41,
    7,    1,    2,    3,   42,   19,   44,   45,   20,   47,
   15,   16,   17,   18,   21,   75,   49,   24,   71,   73,
    8,   76,   67,   74,   82,   77,   49,   84,   85,   11,
    8,   52,   53,   54,   55,   56,   57,   58,   59,   60,
   61,   62,   63,   64,   65,   86,   94,   35,   68,   69,
   70,   33,    1,    2,    3,    4,    5,   81,    6,    0,
   37,    0,    0,    1,    2,    3,    0,    0,    7,    0,
    0,    8,    9,   78,    0,    0,   10,    0,    0,   83,
   11,   23,    8,   87,    0,   89,   90,    0,   91,    0,
   32,   11,   92,    0,    1,    2,    3,    0,    0,    0,
   96,   97,   29,   30,    0,    0,    0,    0,    0,   22,
    0,   39,    0,    8,   39,   39,   39,   39,    0,    0,
   36,   50,   11,   39,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,   66,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
   28,   39,   39,   39,   39,   39,   39,   39,   39,   39,
   39,   39,   39,   39,   39,    0,    0,   39,   39,   39,
    0,    0,    0,    0,    0,    0,    9,   39,    0,    0,
    0,    0,   39,    0,    0,    0,   39,    0,   39,   39,
   39,   39,    0,    0,    0,   39,   39,    0,    0,    0,
    0,    0,   10,    1,    2,    3,    0,   25,   26,   27,
   28,   29,   30,   31,   32,   33,   34,   35,   36,    0,
    0,    0,    8,    0,    0,    0,   37,    0,   20,    0,
   38,   11,   51,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,   27,    0,    0,    7,    7,    7,
    0,    7,    7,    7,    7,    7,    7,    7,    7,    7,
    7,    7,    7,    0,    7,    7,    7,    0,    7,    0,
    7,   23,    7,    0,    7,    7,    7,    8,    8,    8,
    0,    8,    8,    8,    8,    8,    8,    8,    8,    8,
    8,    8,    8,    0,    8,    8,    8,   24,    8,    0,
    8,    0,    0,    0,    8,    8,    8,   37,   37,   37,
    0,   37,   37,   37,   37,   37,   37,   37,   37,   37,
   37,   37,   37,   12,   37,   37,   37,    0,   37,    0,
   37,    0,    0,    0,   37,   37,   37,   32,   32,   32,
    0,   32,   32,   32,   32,   32,   32,   32,   32,   32,
   32,   32,   32,   11,   32,   32,   32,    0,   32,    0,
   32,    0,    0,    0,   32,   32,   32,   36,   36,   36,
    0,   36,   36,   36,   36,   36,   36,   36,   36,   36,
   36,   36,   36,   22,   36,   36,   36,    0,   36,    0,
   36,    0,    0,    0,   36,   36,   36,   28,   28,   28,
    0,   28,   28,   28,   28,   28,   28,   28,   28,   28,
   28,   28,   28,   21,   28,   28,   28,    0,   28,    0,
    0,    0,    0,    0,   28,   28,   28,    9,    9,    9,
    9,    9,    9,    9,    9,    9,    9,    9,    9,   13,
    9,    9,    0,    0,    9,    0,    9,    0,    0,    0,
    9,    0,    9,   10,   10,   10,   10,   10,   10,   10,
   10,   10,   10,   10,   10,   14,   10,   10,    0,    0,
   10,    0,   10,    0,    0,    0,   10,    0,   10,   20,
   20,   20,   20,   20,   20,   20,   20,   20,   20,   20,
   20,   17,   20,   20,    0,    0,   20,    0,   20,    0,
    0,    0,   20,    0,   20,   27,   27,   27,   27,   27,
   27,   27,   27,   27,   27,   27,   27,   18,   27,   27,
    0,    0,   27,    0,   27,    0,    0,    0,   27,    0,
   27,   31,   23,   23,   23,   23,   23,   23,   23,   23,
   23,   23,   23,   23,   15,   23,   23,    0,    0,   23,
    0,   23,    0,    0,    0,   23,    0,   23,   24,   24,
   24,   24,   24,   24,   24,   24,   24,   24,   24,   24,
   16,   24,   24,    0,    0,   24,    0,   24,    0,    0,
    0,   24,    0,   24,   12,   12,   12,   12,   39,    0,
   12,   12,   12,   12,   12,   12,    0,   12,   12,    0,
    0,   12,   38,   12,    0,    0,    0,   12,    0,   12,
    0,    0,    0,    0,   11,   11,   11,   11,   19,    0,
   11,   11,   11,   11,   11,   11,    0,   11,   11,    0,
    0,   11,   30,   11,   26,    0,    0,   11,    0,   11,
    0,    0,    0,    0,   22,   22,   22,   22,   25,    0,
   22,   22,   22,   22,   22,   22,    0,   22,   22,    0,
    0,   22,   29,   22,    0,    0,    0,   22,    0,   22,
    0,    0,    0,    0,   21,   21,   21,   21,    0,    0,
   21,   21,   21,   21,   21,   21,    0,   21,   21,    0,
    0,   21,    0,   21,    0,    0,    0,   21,    0,   21,
    0,    0,    0,    0,    0,    0,   13,   13,   13,   13,
   13,   13,    0,   13,   13,    0,    0,   13,    0,   13,
    0,    0,    0,   13,    0,   13,    0,    0,    0,    0,
    0,    0,   14,   14,   14,   14,   14,   14,    0,   14,
   14,    0,    0,   14,    0,   14,    0,    0,    0,   14,
    0,   14,    0,    0,    0,    0,    0,    0,   17,   17,
   17,   17,   17,   17,    0,   17,   17,    0,    0,   17,
    0,   17,    0,    0,    0,   17,    0,   17,    0,    0,
    0,    0,    0,    0,   18,   18,   18,   18,   18,   18,
    0,   18,   18,    0,    0,   18,    0,   18,    0,    0,
    0,   18,    0,   18,    0,   31,   31,    0,    0,   31,
    0,   15,   15,   15,   15,   15,   15,   31,   15,   15,
    0,    0,   15,    0,   15,    0,    0,    0,   15,    0,
   15,    0,    0,    0,    0,    0,    0,   16,   16,   16,
   16,   16,   16,    0,   16,   16,    0,    0,   16,    0,
   16,    0,    0,    0,   16,    0,   16,    0,    0,    0,
    0,    0,   39,   39,    0,    0,   39,    0,   39,    0,
    0,    0,   39,    0,   39,    0,   38,   38,    0,    0,
   38,    0,   38,    0,    0,    0,   38,    0,   38,    0,
    0,    0,   19,   19,    0,    0,   19,    0,    0,    0,
    0,    0,   19,    0,   19,    0,   30,   30,   26,   26,
   30,    0,   26,    0,    0,    0,   30,    0,   30,    0,
   26,    0,   25,   25,    0,    0,   25,    0,    0,    0,
    0,    0,    0,    0,   25,    0,   29,   29,    0,    0,
   29,    0,    0,    0,    0,    1,    2,    3,   29,   25,
   26,   27,   28,   29,   30,   31,   32,   33,   34,   35,
   36,    0,    0,    0,    8,    0,    0,    0,   37,    0,
    0,    0,   38,   11,   88,    1,    2,    3,    0,   25,
   26,   27,   28,   29,   30,   31,   32,   33,   34,   35,
   36,    0,    0,    0,    8,    0,    0,    0,   37,    0,
    0,    0,   38,   11,   93,    1,    2,    3,    0,   25,
   26,   27,   28,   29,   30,   31,   32,   33,   34,   35,
   36,    0,   43,    0,    8,    0,    0,    0,   37,    0,
    0,    0,   38,   11,    1,    2,    3,    0,   25,   26,
   27,   28,   29,   30,   31,   32,   33,   34,   35,   36,
    0,    0,   79,    8,    0,    0,    0,   37,    0,    0,
    0,   38,   11,    1,    2,    3,    0,   25,   26,   27,
   28,   29,   30,   31,   32,   33,   34,   35,   36,    0,
    0,    0,    8,    0,   80,    0,   37,    0,    0,    0,
   38,   11,    1,    2,    3,    0,   25,   26,   27,   28,
   29,   30,   31,   32,   33,   34,   35,   36,    0,    0,
    0,    8,    0,   95,    0,   37,    0,    0,    0,   38,
   11,    1,    2,    3,    0,   25,   26,   27,   28,   29,
   30,   31,   32,   33,   34,   35,   36,    0,    0,    0,
    8,    0,    0,    0,   37,    0,    0,    0,   38,   11,
    1,    2,    3,    0,   25,   26,   27,   28,   29,   30,
   31,   32,   33,   34,   35,   36,    0,    0,    0,    8,
    0,    0,    0,   37,    0,    1,    2,    3,   11,   25,
   26,   27,   28,   29,   30,   31,   32,   33,   34,   35,
   36,    0,    0,    0,    8,    1,    2,    3,    4,    5,
    0,    6,    0,   11,    0,    0,    0,    0,    0,    0,
    0,    7,    0,    0,    8,    9,    0,    0,    0,   10,
    1,    2,    3,   11,   25,   26,   27,   28,   29,   30,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    8,
    0,    0,    0,    0,    0,    0,    0,    0,   11,
};
}
static short yycheck[];
static { yycheck(); }
static void yycheck() {
yycheck = new short[] {                        282,
    0,  257,  258,  259,  280,  276,  267,  276,  279,  276,
    4,    5,    6,    7,  285,  280,  282,   11,  276,  278,
  276,  286,  285,  280,  267,  285,  282,  276,  276,  285,
    0,   25,   26,   27,   28,   29,   30,   31,   32,   33,
   34,   35,   36,   37,   38,  267,  283,  267,   42,   43,
   44,  278,  257,  258,  259,  260,  261,   71,  263,   -1,
    0,   -1,   -1,  257,  258,  259,   -1,   -1,  273,   -1,
   -1,  276,  277,   67,   -1,   -1,  281,   -1,   -1,   73,
  285,  286,  276,   77,   -1,   79,   80,   -1,   82,   -1,
    0,  285,   86,   -1,  257,  258,  259,   -1,   -1,   -1,
   94,   95,  265,  266,   -1,   -1,   -1,   -1,   -1,   10,
   -1,   12,   -1,  276,   15,   16,   17,   18,   -1,   -1,
    0,   22,  285,   24,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   40,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
    0,   52,   53,   54,   55,   56,   57,   58,   59,   60,
   61,   62,   63,   64,   65,   -1,   -1,   68,   69,   70,
   -1,   -1,   -1,   -1,   -1,   -1,    0,   78,   -1,   -1,
   -1,   -1,   83,   -1,   -1,   -1,   87,   -1,   89,   90,
   91,   92,   -1,   -1,   -1,   96,   97,   -1,   -1,   -1,
   -1,   -1,    0,  257,  258,  259,   -1,  261,  262,  263,
  264,  265,  266,  267,  268,  269,  270,  271,  272,   -1,
   -1,   -1,  276,   -1,   -1,   -1,  280,   -1,    0,   -1,
  284,  285,  286,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,    0,   -1,   -1,  257,  258,  259,
   -1,  261,  262,  263,  264,  265,  266,  267,  268,  269,
  270,  271,  272,   -1,  274,  275,  276,   -1,  278,   -1,
  280,    0,  282,   -1,  284,  285,  286,  257,  258,  259,
   -1,  261,  262,  263,  264,  265,  266,  267,  268,  269,
  270,  271,  272,   -1,  274,  275,  276,    0,  278,   -1,
  280,   -1,   -1,   -1,  284,  285,  286,  257,  258,  259,
   -1,  261,  262,  263,  264,  265,  266,  267,  268,  269,
  270,  271,  272,    0,  274,  275,  276,   -1,  278,   -1,
  280,   -1,   -1,   -1,  284,  285,  286,  257,  258,  259,
   -1,  261,  262,  263,  264,  265,  266,  267,  268,  269,
  270,  271,  272,    0,  274,  275,  276,   -1,  278,   -1,
  280,   -1,   -1,   -1,  284,  285,  286,  257,  258,  259,
   -1,  261,  262,  263,  264,  265,  266,  267,  268,  269,
  270,  271,  272,    0,  274,  275,  276,   -1,  278,   -1,
  280,   -1,   -1,   -1,  284,  285,  286,  257,  258,  259,
   -1,  261,  262,  263,  264,  265,  266,  267,  268,  269,
  270,  271,  272,    0,  274,  275,  276,   -1,  278,   -1,
   -1,   -1,   -1,   -1,  284,  285,  286,  261,  262,  263,
  264,  265,  266,  267,  268,  269,  270,  271,  272,    0,
  274,  275,   -1,   -1,  278,   -1,  280,   -1,   -1,   -1,
  284,   -1,  286,  261,  262,  263,  264,  265,  266,  267,
  268,  269,  270,  271,  272,    0,  274,  275,   -1,   -1,
  278,   -1,  280,   -1,   -1,   -1,  284,   -1,  286,  261,
  262,  263,  264,  265,  266,  267,  268,  269,  270,  271,
  272,    0,  274,  275,   -1,   -1,  278,   -1,  280,   -1,
   -1,   -1,  284,   -1,  286,  261,  262,  263,  264,  265,
  266,  267,  268,  269,  270,  271,  272,    0,  274,  275,
   -1,   -1,  278,   -1,  280,   -1,   -1,   -1,  284,   -1,
  286,    0,  261,  262,  263,  264,  265,  266,  267,  268,
  269,  270,  271,  272,    0,  274,  275,   -1,   -1,  278,
   -1,  280,   -1,   -1,   -1,  284,   -1,  286,  261,  262,
  263,  264,  265,  266,  267,  268,  269,  270,  271,  272,
    0,  274,  275,   -1,   -1,  278,   -1,  280,   -1,   -1,
   -1,  284,   -1,  286,  261,  262,  263,  264,    0,   -1,
  267,  268,  269,  270,  271,  272,   -1,  274,  275,   -1,
   -1,  278,    0,  280,   -1,   -1,   -1,  284,   -1,  286,
   -1,   -1,   -1,   -1,  261,  262,  263,  264,    0,   -1,
  267,  268,  269,  270,  271,  272,   -1,  274,  275,   -1,
   -1,  278,    0,  280,    0,   -1,   -1,  284,   -1,  286,
   -1,   -1,   -1,   -1,  261,  262,  263,  264,    0,   -1,
  267,  268,  269,  270,  271,  272,   -1,  274,  275,   -1,
   -1,  278,    0,  280,   -1,   -1,   -1,  284,   -1,  286,
   -1,   -1,   -1,   -1,  261,  262,  263,  264,   -1,   -1,
  267,  268,  269,  270,  271,  272,   -1,  274,  275,   -1,
   -1,  278,   -1,  280,   -1,   -1,   -1,  284,   -1,  286,
   -1,   -1,   -1,   -1,   -1,   -1,  267,  268,  269,  270,
  271,  272,   -1,  274,  275,   -1,   -1,  278,   -1,  280,
   -1,   -1,   -1,  284,   -1,  286,   -1,   -1,   -1,   -1,
   -1,   -1,  267,  268,  269,  270,  271,  272,   -1,  274,
  275,   -1,   -1,  278,   -1,  280,   -1,   -1,   -1,  284,
   -1,  286,   -1,   -1,   -1,   -1,   -1,   -1,  267,  268,
  269,  270,  271,  272,   -1,  274,  275,   -1,   -1,  278,
   -1,  280,   -1,   -1,   -1,  284,   -1,  286,   -1,   -1,
   -1,   -1,   -1,   -1,  267,  268,  269,  270,  271,  272,
   -1,  274,  275,   -1,   -1,  278,   -1,  280,   -1,   -1,
   -1,  284,   -1,  286,   -1,  274,  275,   -1,   -1,  278,
   -1,  267,  268,  269,  270,  271,  272,  286,  274,  275,
   -1,   -1,  278,   -1,  280,   -1,   -1,   -1,  284,   -1,
  286,   -1,   -1,   -1,   -1,   -1,   -1,  267,  268,  269,
  270,  271,  272,   -1,  274,  275,   -1,   -1,  278,   -1,
  280,   -1,   -1,   -1,  284,   -1,  286,   -1,   -1,   -1,
   -1,   -1,  274,  275,   -1,   -1,  278,   -1,  280,   -1,
   -1,   -1,  284,   -1,  286,   -1,  274,  275,   -1,   -1,
  278,   -1,  280,   -1,   -1,   -1,  284,   -1,  286,   -1,
   -1,   -1,  274,  275,   -1,   -1,  278,   -1,   -1,   -1,
   -1,   -1,  284,   -1,  286,   -1,  274,  275,  274,  275,
  278,   -1,  278,   -1,   -1,   -1,  284,   -1,  286,   -1,
  286,   -1,  274,  275,   -1,   -1,  278,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,  286,   -1,  274,  275,   -1,   -1,
  278,   -1,   -1,   -1,   -1,  257,  258,  259,  286,  261,
  262,  263,  264,  265,  266,  267,  268,  269,  270,  271,
  272,   -1,   -1,   -1,  276,   -1,   -1,   -1,  280,   -1,
   -1,   -1,  284,  285,  286,  257,  258,  259,   -1,  261,
  262,  263,  264,  265,  266,  267,  268,  269,  270,  271,
  272,   -1,   -1,   -1,  276,   -1,   -1,   -1,  280,   -1,
   -1,   -1,  284,  285,  286,  257,  258,  259,   -1,  261,
  262,  263,  264,  265,  266,  267,  268,  269,  270,  271,
  272,   -1,  274,   -1,  276,   -1,   -1,   -1,  280,   -1,
   -1,   -1,  284,  285,  257,  258,  259,   -1,  261,  262,
  263,  264,  265,  266,  267,  268,  269,  270,  271,  272,
   -1,   -1,  275,  276,   -1,   -1,   -1,  280,   -1,   -1,
   -1,  284,  285,  257,  258,  259,   -1,  261,  262,  263,
  264,  265,  266,  267,  268,  269,  270,  271,  272,   -1,
   -1,   -1,  276,   -1,  278,   -1,  280,   -1,   -1,   -1,
  284,  285,  257,  258,  259,   -1,  261,  262,  263,  264,
  265,  266,  267,  268,  269,  270,  271,  272,   -1,   -1,
   -1,  276,   -1,  278,   -1,  280,   -1,   -1,   -1,  284,
  285,  257,  258,  259,   -1,  261,  262,  263,  264,  265,
  266,  267,  268,  269,  270,  271,  272,   -1,   -1,   -1,
  276,   -1,   -1,   -1,  280,   -1,   -1,   -1,  284,  285,
  257,  258,  259,   -1,  261,  262,  263,  264,  265,  266,
  267,  268,  269,  270,  271,  272,   -1,   -1,   -1,  276,
   -1,   -1,   -1,  280,   -1,  257,  258,  259,  285,  261,
  262,  263,  264,  265,  266,  267,  268,  269,  270,  271,
  272,   -1,   -1,   -1,  276,  257,  258,  259,  260,  261,
   -1,  263,   -1,  285,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,  273,   -1,   -1,  276,  277,   -1,   -1,   -1,  281,
  257,  258,  259,  285,  261,  262,  263,  264,  265,  266,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  276,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  285,
};
}
final static short YYFINAL=12;
final static short YYMAXTOKEN=291;
final static String yyname[] = {
"end-of-file",null,null,null,null,null,null,null,null,null,null,null,null,null,
null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,
null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,
null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,
null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,
null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,
null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,
null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,
null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,
null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,
null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,
null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,
null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,
null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,
null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,
null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,
null,null,null,"BOOL","INT","FLOAT","NOT","MINUS","PLUS","MINUS_DOT","PLUS_DOT",
"AST_DOT","SLASH_DOT","EQUAL","LESS_GREATER","LESS_EQUAL","GREATER_EQUAL",
"LESS","GREATER","IF","THEN","ELSE","IDENT","LET","IN","REC","COMMA",
"ARRAY_CREATE","DOT","LESS_MINUS","SEMICOLON","LPAREN","RPAREN","EOF",
"prec_let","prec_if","prec_unary_minus","prec_app",
};
final static String yyrule[] = {
"$accept : exp",
"simple_exp : LPAREN exp RPAREN",
"simple_exp : LPAREN RPAREN",
"simple_exp : BOOL",
"simple_exp : INT",
"simple_exp : FLOAT",
"simple_exp : IDENT",
"simple_exp : simple_exp DOT LPAREN exp RPAREN",
"exp : simple_exp",
"exp : NOT exp",
"exp : MINUS exp",
"exp : exp PLUS exp",
"exp : exp MINUS exp",
"exp : exp EQUAL exp",
"exp : exp LESS_GREATER exp",
"exp : exp LESS exp",
"exp : exp GREATER exp",
"exp : exp LESS_EQUAL exp",
"exp : exp GREATER_EQUAL exp",
"exp : IF exp THEN exp ELSE exp",
"exp : MINUS_DOT exp",
"exp : exp PLUS_DOT exp",
"exp : exp MINUS_DOT exp",
"exp : exp AST_DOT exp",
"exp : exp SLASH_DOT exp",
"exp : LET IDENT EQUAL exp IN exp",
"exp : LET REC fundef IN exp",
"exp : exp actual_args",
"exp : elems",
"exp : LET LPAREN pat RPAREN EQUAL exp IN exp",
"exp : simple_exp DOT LPAREN exp RPAREN LESS_MINUS exp",
"exp : exp SEMICOLON exp",
"exp : ARRAY_CREATE simple_exp simple_exp",
"fundef : IDENT formal_args EQUAL exp",
"formal_args : IDENT formal_args",
"formal_args : IDENT",
"actual_args : actual_args simple_exp",
"actual_args : simple_exp",
"elems : elems COMMA exp",
"elems : exp COMMA exp",
"pat : pat COMMA IDENT",
"pat : IDENT COMMA IDENT",
};

//#line 177 "parser.y"
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
//#line 543 "Parser.java"
//###############################################################
// method: yylexdebug : check lexer state
//###############################################################
void yylexdebug(int state,int ch)
{
String s=null;
  if (ch < 0) ch=0;
  if (ch <= YYMAXTOKEN) //check index bounds
     s = yyname[ch];    //now get it
  if (s==null)
    s = "illegal-symbol";
  debug("state "+state+", reading "+ch+" ("+s+")");
}





//The following are now global, to aid in error reporting
int yyn;       //next next thing to do
int yym;       //
int yystate;   //current parsing state from state table
String yys;    //current token string


//###############################################################
// method: yyparse : parse input and execute indicated items
//###############################################################
int yyparse()
{
boolean doaction;
  init_stacks();
  yynerrs = 0;
  yyerrflag = 0;
  yychar = -1;          //impossible char forces a read
  yystate=0;            //initial state
  state_push(yystate);  //save it
  val_push(yylval);     //save empty value
  while (true) //until parsing is done, either correctly, or w/error
    {
    doaction=true;
    if (yydebug) debug("loop"); 
    //#### NEXT ACTION (from reduction table)
    for (yyn=yydefred[yystate];yyn==0;yyn=yydefred[yystate])
      {
      if (yydebug) debug("yyn:"+yyn+"  state:"+yystate+"  yychar:"+yychar);
      if (yychar < 0)      //we want a char?
        {
        yychar = yylex();  //get next token
        if (yydebug) debug(" next yychar:"+yychar);
        //#### ERROR CHECK ####
        if (yychar < 0)    //it it didn't work/error
          {
          yychar = 0;      //change it to default string (no -1!)
          if (yydebug)
            yylexdebug(yystate,yychar);
          }
        }//yychar<0
      yyn = yysindex[yystate];  //get amount to shift by (shift index)
      if ((yyn != 0) && (yyn += yychar) >= 0 &&
          yyn <= YYTABLESIZE && yycheck[yyn] == yychar)
        {
        if (yydebug)
          debug("state "+yystate+", shifting to state "+yytable[yyn]);
        //#### NEXT STATE ####
        yystate = yytable[yyn];//we are in a new state
        state_push(yystate);   //save it
        val_push(yylval);      //push our lval as the input for next rule
        yychar = -1;           //since we have 'eaten' a token, say we need another
        if (yyerrflag > 0)     //have we recovered an error?
           --yyerrflag;        //give ourselves credit
        doaction=false;        //but don't process yet
        break;   //quit the yyn=0 loop
        }

    yyn = yyrindex[yystate];  //reduce
    if ((yyn !=0 ) && (yyn += yychar) >= 0 &&
            yyn <= YYTABLESIZE && yycheck[yyn] == yychar)
      {   //we reduced!
      if (yydebug) debug("reduce");
      yyn = yytable[yyn];
      doaction=true; //get ready to execute
      break;         //drop down to actions
      }
    else //ERROR RECOVERY
      {
      if (yyerrflag==0)
        {
        yyerror("syntax error");
        yynerrs++;
        }
      if (yyerrflag < 3) //low error count?
        {
        yyerrflag = 3;
        while (true)   //do until break
          {
          if (stateptr<0)   //check for under & overflow here
            {
            yyerror("stack underflow. aborting...");  //note lower case 's'
            return 1;
            }
          yyn = yysindex[state_peek(0)];
          if ((yyn != 0) && (yyn += YYERRCODE) >= 0 &&
                    yyn <= YYTABLESIZE && yycheck[yyn] == YYERRCODE)
            {
            if (yydebug)
              debug("state "+state_peek(0)+", error recovery shifting to state "+yytable[yyn]+" ");
            yystate = yytable[yyn];
            state_push(yystate);
            val_push(yylval);
            doaction=false;
            break;
            }
          else
            {
            if (yydebug)
              debug("error recovery discarding state "+state_peek(0)+" ");
            if (stateptr<0)   //check for under & overflow here
              {
              yyerror("Stack underflow. aborting...");  //capital 'S'
              return 1;
              }
            state_pop();
            val_pop();
            }
          }
        }
      else            //discard this token
        {
        if (yychar == 0)
          return 1; //yyabort
        if (yydebug)
          {
          yys = null;
          if (yychar <= YYMAXTOKEN) yys = yyname[yychar];
          if (yys == null) yys = "illegal-symbol";
          debug("state "+yystate+", error recovery discards token "+yychar+" ("+yys+")");
          }
        yychar = -1;  //read another
        }
      }//end error recovery
    }//yyn=0 loop
    if (!doaction)   //any reason not to proceed?
      continue;      //skip action
    yym = yylen[yyn];          //get count of terminals on rhs
    if (yydebug)
      debug("state "+yystate+", reducing "+yym+" by rule "+yyn+" ("+yyrule[yyn]+")");
    if (yym>0)                 //if count of rhs not 'nil'
      yyval = val_peek(yym-1); //get current semantic value
    yyval = dup_yyval(yyval); //duplicate yyval if ParserVal is used as semantic value
    switch(yyn)
      {
//########## USER-SUPPLIED ACTIONS ##########
case 1:
//#line 60 "parser.y"
{ yyval.obj = val_peek(1).obj; }
break;
case 2:
//#line 62 "parser.y"
{ yyval.obj = new Unit(); }
break;
case 3:
//#line 64 "parser.y"
{ yyval.obj = new Bool((Boolean)val_peek(0).obj); }
break;
case 4:
//#line 66 "parser.y"
{ yyval.obj = new Int(val_peek(0).ival); }
break;
case 5:
//#line 68 "parser.y"
{ yyval.obj = new Syntax.Float(val_peek(0).dval); }
break;
case 6:
//#line 69 "parser.y"
{ yyval.obj = new Var((String)val_peek(0).sval); }
break;
case 7:
//#line 71 "parser.y"
{ yyval.obj = new Get((T)val_peek(4).obj, (T)val_peek(1).obj); }
break;
case 8:
//#line 75 "parser.y"
{ yyval.obj = val_peek(0).obj; }
break;
case 9:
//#line 78 "parser.y"
{ yyval.obj = new Not((T)val_peek(0).obj); }
break;
case 10:
//#line 81 "parser.y"
{
        if (val_peek(0).obj instanceof Syntax.Float) {
            yyval.obj = new Syntax.Float(-((Syntax.Float)val_peek(0).obj).a());
            /* -1.23Ç»Ç«ÇÕå^ÉGÉâÅ[Ç≈ÇÕÇ»Ç¢ÇÃÇ≈ï àµÇ¢*/
        } else {
            yyval.obj = new Neg((T)val_peek(0).obj);
        }
    }
break;
case 11:
//#line 90 "parser.y"
{ yyval.obj = new Add((T)val_peek(2).obj, (T)val_peek(0).obj); }
break;
case 12:
//#line 92 "parser.y"
{ yyval.obj = new Sub((T)val_peek(2).obj, (T)val_peek(0).obj); }
break;
case 13:
//#line 94 "parser.y"
{ yyval.obj = new Eq((T)val_peek(2).obj, (T)val_peek(0).obj); }
break;
case 14:
//#line 96 "parser.y"
{ yyval.obj = new Not(new Eq((T)val_peek(2).obj, (T)val_peek(0).obj)); }
break;
case 15:
//#line 98 "parser.y"
{ yyval.obj = new Not(new LE((T)val_peek(0).obj, (T)val_peek(2).obj)); }
break;
case 16:
//#line 100 "parser.y"
{ yyval.obj = new Not(new LE((T)val_peek(2).obj, (T)val_peek(0).obj)); }
break;
case 17:
//#line 102 "parser.y"
{ yyval.obj = new LE((T)val_peek(2).obj, (T)val_peek(0).obj); }
break;
case 18:
//#line 104 "parser.y"
{ yyval.obj = new LE((T)val_peek(0).obj, (T)val_peek(2).obj); }
break;
case 19:
//#line 107 "parser.y"
{ yyval.obj = new If((T)val_peek(4).obj, (T)val_peek(2).obj, (T)val_peek(0).obj); }
break;
case 20:
//#line 110 "parser.y"
{ yyval.obj = new FNeg((T)val_peek(0).obj); }
break;
case 21:
//#line 112 "parser.y"
{ yyval.obj = new FAdd((T)val_peek(2).obj, (T)val_peek(0).obj); }
break;
case 22:
//#line 114 "parser.y"
{ yyval.obj = new FSub((T)val_peek(2).obj, (T)val_peek(0).obj); }
break;
case 23:
//#line 116 "parser.y"
{ yyval.obj = new FMul((T)val_peek(2).obj, (T)val_peek(0).obj); }
break;
case 24:
//#line 118 "parser.y"
{ yyval.obj = new FDiv((T)val_peek(2).obj, (T)val_peek(0).obj); }
break;
case 25:
//#line 121 "parser.y"
{ yyval.obj = let(addtyp((String)val_peek(4).sval), (T)val_peek(2).obj, (T)val_peek(0).obj); }
break;
case 26:
//#line 124 "parser.y"
{ yyval.obj = new LetRec((Fundef)val_peek(2).obj, (T)val_peek(0).obj); }
break;
case 27:
//#line 127 "parser.y"
{ yyval.obj = app((T)val_peek(1).obj, (scala.List<T>)val_peek(0).obj); }
break;
case 28:
//#line 129 "parser.y"
{ System.out.println(val_peek(0).obj); yyval.obj = tuple((scala.List<T>)val_peek(0).obj); }
break;
case 29:
//#line 131 "parser.y"
{ yyval.obj = letTuple((scala.List<scala.Tuple2<String,Type.T>>)val_peek(5).obj, (T)val_peek(2).obj, (T)val_peek(0).obj); }
break;
case 30:
//#line 133 "parser.y"
{ yyval.obj = new Put((T)val_peek(6).obj, (T)val_peek(3).obj, (T)val_peek(0).obj); }
break;
case 31:
//#line 135 "parser.y"
{ yyval.obj = let(tuple2(Id.gentmp(new Type.Unit()), new Type.Unit()), (T)val_peek(2).obj, (T)val_peek(0).obj); }
break;
case 32:
//#line 138 "parser.y"
{ yyval.obj = new Array((T)val_peek(1).obj, (T)val_peek(0).obj); }
break;
case 33:
//#line 148 "parser.y"
{ yyval.obj = fundef(addtyp((String)val_peek(3).sval), (scala.List<scala.Tuple2<String,Type.T>>)val_peek(2).obj, (T)val_peek(0).obj); }
break;
case 34:
//#line 152 "parser.y"
{ yyval.obj = addList2(addtyp((String)val_peek(1).sval),(scala.List<scala.Tuple2<String,Type.T>>)val_peek(0).obj); }
break;
case 35:
//#line 154 "parser.y"
{ yyval.obj = list2(addtyp((String)val_peek(0).sval)); }
break;
case 36:
//#line 159 "parser.y"
{ yyval.obj = concatList((scala.List<T>)val_peek(1).obj, list((T)val_peek(0).obj)); }
break;
case 37:
//#line 162 "parser.y"
{ yyval.obj = list((T)val_peek(0).obj); }
break;
case 38:
//#line 166 "parser.y"
{ yyval.obj = concatList((scala.List<T>)val_peek(2).obj, list((T)val_peek(0).obj)); }
break;
case 39:
//#line 168 "parser.y"
{ yyval.obj = addList((T)val_peek(2).obj,list((T)val_peek(0).obj)); }
break;
case 40:
//#line 172 "parser.y"
{ yyval.obj = concatList2((scala.List<scala.Tuple2<String,Type.T>>)val_peek(2).obj, list2(addtyp((String)val_peek(0).sval))); }
break;
case 41:
//#line 174 "parser.y"
{ yyval.obj = addList2(addtyp((String)val_peek(2).sval),list2(addtyp((String)val_peek(0).sval))); }
break;
//#line 863 "Parser.java"
//########## END OF USER-SUPPLIED ACTIONS ##########
    }//switch
    //#### Now let's reduce... ####
    if (yydebug) debug("reduce");
    state_drop(yym);             //we just reduced yylen states
    yystate = state_peek(0);     //get new state
    val_drop(yym);               //corresponding value drop
    yym = yylhs[yyn];            //select next TERMINAL(on lhs)
    if (yystate == 0 && yym == 0)//done? 'rest' state and at first TERMINAL
      {
      if (yydebug) debug("After reduction, shifting from state 0 to state "+YYFINAL+"");
      yystate = YYFINAL;         //explicitly say we're done
      state_push(YYFINAL);       //and save it
      val_push(yyval);           //also save the semantic value of parsing
      if (yychar < 0)            //we want another character?
        {
        yychar = yylex();        //get next character
        if (yychar<0) yychar=0;  //clean, if necessary
        if (yydebug)
          yylexdebug(yystate,yychar);
        }
      if (yychar == 0)          //Good exit (if lex returns 0 ;-)
         break;                 //quit the loop--all DONE
      }//if yystate
    else                        //else not done yet
      {                         //get next state and push, for next yydefred[]
      yyn = yygindex[yym];      //find out where to go
      if ((yyn != 0) && (yyn += yystate) >= 0 &&
            yyn <= YYTABLESIZE && yycheck[yyn] == yystate)
        yystate = yytable[yyn]; //get new state
      else
        yystate = yydgoto[yym]; //else go to new defred
      if (yydebug) debug("after reduction, shifting from state "+state_peek(0)+" to state "+yystate+"");
      state_push(yystate);     //going again, so push state & val...
      val_push(yyval);         //for next action
      }
    }//main loop
  return 0;//yyaccept!!
}
//## end of method parse() ######################################



//## run() --- for Thread #######################################
/**
 * A default run method, used for operating this parser
 * object in the background.  It is intended for extending Thread
 * or implementing Runnable.  Turn off with -Jnorun .
 */
public void run()
{
  yyparse();
}
//## end of method run() ########################################



//## Constructors ###############################################
/**
 * Default constructor.  Turn off with -Jnoconstruct .

 */
public Parser()
{
  //nothing to do
}


/**
 * Create a parser, setting the debug to true or false.
 * @param debugMe true for debugging, false for no debug.
 */
public Parser(boolean debugMe)
{
  yydebug=debugMe;
}
//###############################################################



}
//################### END OF CLASS ##############################
