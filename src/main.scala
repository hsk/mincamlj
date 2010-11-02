/*
メインルーチン
 
次にMinCamlのメインルーチンmain.mlを見てください。末尾のlet () = ...という部分から、コンパイラの実行が開始されます。
 
まず、OCamlの標準ライブラリArgを利用して、オプションを処理しています。-inlineでインライン展開する関数のサイズを、-iterで最適化処理のループ回数を指定します（詳しくは後で述べます）。
 
それ以外のコマンドライン引数は、コンパイルするプログラムの名前と解釈され、関数Main.fileにより「プログラム名.ml」というMinCamlソースコードから「プログラム名.s」というSPARCアセンブリが生成されます。
 
なお、デバッグや実験のために、引数の文字列をコンパイルして、標準出力に結果を表示するMain.stringという関数も用意されています。
 
main.mlの中核は関数Main.lexbufです。引数として受け取ったバッファに対し、順に字句解析（Lexer.token）、構文解析（Parser.exp）、型推論（Typing.f）、K正規化（KNormal.f）、α変換（Alpha.f）、最適化（iter）、クロージャ変換（Closure.f）、仮想マシンコード生成（Virtual.f）、SPARCの13 bit即値最適化（Simm13.f）、レジスタ割り当て（RegAlloc.f）、アセンブリ生成（Emit.f）を行います。
 
最適化関数iterは、β簡約（Beta.f）、ネストしたletの簡約（Assoc.f）、インライン展開（Inline.f）、定数畳み込み（ConstFold.f）、不要定義削除（Elim.f）の5つを、-iterで指定した回数を上限として、結果が不変になるまで適用します。
 
これらの処理の内容については、以下で説明していきます。
 
 
MinCamlの構文と型
 
MinCamlはMLのサブセットです。優先順位や括弧などの細かいことを別にすると、以下のような構文をもっています。
 
e ::=	式
  c	定数
  op(e1, ..., en)	プリミティブ演算
  if e1 then e2 else e3	条件分岐
  let x = e1 in e2	変数定義
  x	変数の読み出し
  let rec x y1 ... yn = e1 in e2	再帰関数定義
  e e1 ... en	関数呼び出し
  (e1, ..., en)	組の作成
  let (x1, ..., xn) = e1 in e2	組の読み出し
  Array.create e1 e2	配列の作成
  e1.(e2)	配列の読み出し
  e1.(e2) <- e3	配列への書き込み
 
これをMLのデータ型Syntax.tとして表したモジュールがsyntax.mlです。ただし、letやlet recのように、新しい変数が定義される式では、その型も（上の構文にはありませんが）含まれています。この型を表すType.tはtype.mlで定義されています。
 
T ::=	型
  π	プリミティブ型
  T1 → ... → Tn → T	関数型
  T1 × ... × Tn	 組の型
  T array	配列型
  α	型変数
 
最後の「型変数」は、型推論のところで使用します。
 
なおMinCamlコンパイラでは、いわゆるカリー化関数の部分適用を自動ではサポートしていません。つまり、関数呼び出しではすべての引数を与えなければいけません。部分適用をしたいときは、たとえばlet rec f_123 x = f 123 x in f_123のように自分で関数定義をする必要があります（Scheme言語を知っている人は、それと同じだと思ってください）。
 
また、命令型プログラミングに必要な参照(reference)もありませんが、これは要素が一個しかないような配列で代用できます。具体的には、ref eはArray.create 1 e、!eはe.(0)、e1 := e2はe1.(0) <- e2とすればOKです。


コンパイラの方針
 
一般にコンパイルとは、ある高水準言語のプログラムを、より低水準な言語に変換することです。たとえば、二つの非負整数の最大公約数を求めるMinCaml関数
 
let rec gcd m n =
  if m = 0 then n else
  if m <= n then gcd m (n - m) else
  gcd n (m - n)
 
は、以下のようなSPARCアセンブリにコンパイルされます。
 
gcd.7:
        cmp     %i2, 0
        bne     be_else.18
        nop
        mov     %i3, %i2
        retl
        nop
be_else.18:
        cmp     %i2, %i3
        bg      ble_else.19
        nop
        sub     %i3, %i2, %i3
        b       gcd.7
        nop
ble_else.19:
        sub     %i2, %i3, %o5
        mov     %i3, %i2
        mov     %o5, %i3
        b       gcd.7
        nop
 
これは一見すると、ものすごいギャップです。MinCamlコンパイラでは適切な中間言語を設定し、単純な変換を順番に適用していくことにより、このギャップを一つずつ埋めていきます。MinCamlとSPARCアセンブリの主なギャップは次の5つです。
型。MinCamlには型があり、型チェックが行われますが、アセンブリにはそのような仕組みがありません。
ネストした式。たとえばMinCamlでは1+2-(3+(4-5))のように、いくらでも複雑な式を書くことができますが、アセンブリでは一つの命令につき一つの演算しかできません。
ネストした関数定義。MinCamlでは
let rec make_adder x =
  let rec adder y = x + y in
  adder in
(make_adder 3) 7
のように関数の中で別の関数を定義できますが、アセンブリではトップレベルの「ラベル」しか使用できません。
MinCamlには組や配列などのデータ構造がありますが、アセンブリにはありません。
MinCamlではいくつでも変数を使用できますが、アセンブリには有限のレジスタしかありません。
 
これらのギャップを埋めるために、MinCamlでは
型推論
K正規化
クロージャ変換
仮想マシンコード生成
レジスタ割り当て
 
といった処理を順番に行います。以下では、そのような変換や各種最適化について説明していきます。
 
*/
package mincaml;
import java.io._;

object Main {
	var limit = 1000

	/**
	 * 最適化
	 */
	def optimize(n:Int, e:KNormal.T):KNormal.T = {
		if (n == 0) e else {
			val e1 = Elim(ConstFold(Inline(Assoc(Beta(e)))))
			if (e == e1) e else optimize((n - 1), edash)
		}
	}

	/**
	 * バッファをコンパイルしてチャンネルへ出力する
	 */
	def lexbuf(outchan:PrintWriter, inchan:FileReader):Unit = {

		val yyparser = new Parser(inchan)
		yyparser.yyparse()

		Id.counter = 0
		Typing.extenv = Map()

		val ast = yyparser.yyval.obj.asInstanceOf[Syntax.T]; println("ast "+ast)
		val typedAst = Typing(ast); println("typedAst "+typedAst)
		val knormal = KNormal(typedAst); println("knormal "+knormal)
		val alpha = Alpha(knormal); println("alpha "+ alpha)
		val opt = optimize(limit, alpha);println("optimize "+opt)
		val closure = Closure(opt); println("closure "+closure)
		val virtual = Virtual(closure); println("virtual "+virtual)
		val simm13 = Simm13(virtual); println("simm13 "+simm13)
		val regialloc = RegAlloc(simm13); println("regalloc "+ regialloc)
		Emit_x86(outchan, regialloc)

		outchan.close()
	}

	/**
	 * ファイルをコンパイルしてファイルに出力する
	 */
	def file(f:String):Unit = {
		println("file="+f)
		val inchan = new FileReader(f + ".ml")
		val outchan = new FileWriter(f + ".s")
		try {
			lexbuf(new PrintWriter(new BufferedWriter(outchan)), inchan)
			inchan.close()
			outchan.close()
			gcc(f + ".s", f)
		} catch {
			case e => inchan.close(); outchan.close(); throw e
		}
	}

	def gcc(file:String, out:String):Unit = {
		try {
			var rt = Runtime.getRuntime()
			var p = rt.exec("gcc -o "+out+" "+file+" libmincaml_x86.s stub.c")
			var br = new BufferedReader(new InputStreamReader(p.getInputStream()))
			var result:String = ""
			while (result != null) {
				result = br.readLine()
				if(result != null) {
					System.out.println(result)
				}
			}
			br = new BufferedReader(new InputStreamReader(p.getErrorStream()))
			result = ""
			while (result != null) {
				result = br.readLine()
				if(result != null) {
					System.out.println(result)
				}
			}

			p.waitFor();
		} catch {
			case ex:IOException =>
				ex.printStackTrace()
		}
	}

	/**
	 * ここからコンパイラの実行が開始される
	 */
	def main(argv:Array[String]):Unit = {

		def arg(n:Int,l:List[String]):List[String] = {

			if (n == argv.length) {
				if(l.size==0)throw new Exception()
				l
			} else {
				argv(n) match {
				case "-inline" => Inline.threshold = argv(n+1).toInt; arg(n+2, l)
				case "-iter" =>   limit = argv(n+1).toInt; arg(n+2, l)
				case s => arg(n+1, s::l)
				}
			}
		}
		var files = try {
			arg(0, List())
		} catch {
			case _ => println("Mitou Min-Caml Compiler (C) Eijiro Sumii\n" +"usage: mincaml [-inline m] [-iter n] ...filenames without \".ml\"..."); return
		}
		files.foreach{file(_)}
	}
}
