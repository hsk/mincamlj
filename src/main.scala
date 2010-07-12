package mincaml;
import java.io._;

object Main {
	var limit = 1000
	// 最適化処理をくりかえす (caml2html: main_iter)
	def iter(n:Int, e:KNormal.T):KNormal.T = {
		println("iteration n@.");
		if (n == 0) {
			e 
		} else {
			val edash = Elim.f(ConstFold.f(Inline.f(Assoc.f(Beta.f(e)))));
			if (e == edash) {
				e
			} else {
				iter((n - 1), edash)
			}
		}
	}

	// バッファをコンパイルしてチャンネルへ出力する (caml2html: main_lexbuf)
	def lexbuf(outchan:PrintWriter, inchan:FileReader):Unit = {

		val yyparser = new Parser(inchan);
		yyparser.yyparse();

		Id.counter = 0;
		Typing.extenv = Map();


		val ast = yyparser.yyval.obj.asInstanceOf[Syntax.T]; println("ast "+ast)
		val typedAst = Typing.f(ast); println("typedAst "+typedAst)
		val knormal = KNormal.f(typedAst); println("knormal "+knormal)
		val alpha = Alpha.f(knormal); println("alpha "+ alpha);
		val iter1 = iter(limit, alpha);println("iter "+iter1);
		val closure = Closure.f(iter1); println("closure "+closure);
		val virtual = Virtual.f(closure); println("virtual "+virtual);
		val simm13 = Simm13.f(virtual); println("simm13 "+simm13);
		val regialloc = RegAlloc.f(simm13); println("regalloc "+ regialloc);
		Emit_x86.f(outchan, regialloc)
		outchan.close();
	}
/*
	// 文字列をコンパイルして標準出力に表示する (caml2html: main_string)
//	def _string(s) = lexbuf(stdout, Lexing.from_string(s))
*/

	// ファイルをコンパイルしてファイルに出力する (caml2html: main_file)
	def file(f:String):Unit = {
		println("file="+f);
		val inchan = new FileReader(f + ".ml") ;
		val outchan = new FileWriter(f + ".s");
		try {
			lexbuf(new PrintWriter(new BufferedWriter(outchan)), inchan);
			inchan.close();
			outchan.close();
			gcc(f + ".s");
		} catch {
			case e => inchan.close(); outchan.close(); throw e;
		}
	}
	def gcc(file:String) {
 try {
    var rt = Runtime.getRuntime();
    var p = rt.exec("gcc "+file+" libmincaml_x86.s stub.c");
    var br = new BufferedReader(new InputStreamReader(p.getInputStream()));
    var result:String = "";
      while (result != null) {
		result = br.readLine();
		if(result != null) {
			System.out.println(result);
		}
      }
    br = new BufferedReader(new InputStreamReader(p.getErrorStream()));
    result = "";
      while (result != null) {
		result = br.readLine();
		if(result != null) {
			System.out.println(result);
		}
      }

      p.waitFor();
  } catch {
  case ex:IOException =>
    ex.printStackTrace();
  }	
	
	}

	// ここからコンパイラの実行が開始される (caml2html: main_entry)
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
			arg(0, List());
		} catch {
			case _ => println("Mitou Min-Caml Compiler (C) Eijiro Sumii\n" +"usage: mincaml [-inline m] [-iter n] ...filenames without \".ml\"..."); return
		}
		files.foreach{file(_)}
	}
}
