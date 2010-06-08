val limit = 1000

// 最適化処理をくりかえす (caml2html: main_iter)
def iter(n, e) = {
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
def lexbuf(outchan, l) = {
	Id.counter = 0;
	Typing.extenv = M.empty();
	Emit.f(outchan,
		RegAlloc.f(
			Simm13.f(
				Virtual.f(
					Closure.f(
						iter(limit,
							Alpha.f(
								KNormal.f(
									Typing.f(
										Parser.exp(Lexer.token, l)))))))))
}

// 文字列をコンパイルして標準出力に表示する (caml2html: main_string)
def _string(s) = lexbuf(stdout, Lexing.from_string(s))

// ファイルをコンパイルしてファイルに出力する (caml2html: main_file)
def file(f) = {
	val inchan = open_in (f + ".ml");
	val outchan = open_out (f ^ ".s");
	try {
		lexbuf(outchan, Lexing.from_channel(inchan));
		close_in(inchan);
		close_out(outchan);
	} catch {
		case e => close_in(inchan); close_out(outchan); throw e;
	}
}

// ここからコンパイラの実行が開始される (caml2html: main_entry)
def main(argv:Array[String]) = {
	val files = List();
	Arg.parse(
		List(
			("-inline", Arg.Int(i => Inline.threshold = i), "maximum size of functions inlined"),
			("-iter",   Arg.Int(i => limit = i),            "maximum number of optimizations iterated")
		),
		s => files = files ::: List(s),
		"Mitou Min-Caml Compiler (C) Eijiro Sumii\n" +"usage: "+argv(0)+" [-inline m] [-iter n] ...filenames without \".ml\"..."
	);
	List.iter(
		f => ignore(file(f)),
		files
	)
}
