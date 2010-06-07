open KNormal

// インライン展開する関数の最大サイズ (caml2html: inline_threshold)
var threshold = 0 // Mainで-inlineオプションによりセットされる

def size(e) = e match {
	case IfEq(_, _, e1, e2)         => 1 + size(e1) + size (e2)
	case IfLE(_, _, e1, e2)         => 1 + size(e1) + size (e2)
	case Let(_, e1, e2)             => 1 + size(e1) + size (e2)
	case LetRec(Fundef(_,_,e1), e2) => 1 + size(e1) + size (e2)
	case LetTuple(_, _, e)          => 1 + size(e)
	case _                          => 1
}

// インライン展開ルーチン本体 (caml2html: inline_g)
def g(env,e) = e match {
	case IfEq(x, y, e1, e2) => IfEq(x, y, g(env, e1), g(env, e2))
	case IfLE(x, y, e1, e2) => IfLE(x, y, g(env, e1), g(env, e2))
	case Let(xt, e1, e2)    => Let(xt, g(env, e1), g(env, e2))

	// 関数定義の場合 (caml2html: inline_letrec)
	case LetRec(Fundef((x, t), yts, e1), e2) => 
		val env = if (size(e1) > threshold) env else M.add(x,(yts, e1), env);
		LetRec(Fundef((x, t), yts, g(env, e1)), g(env, e2))
	// 関数適用の場合 (caml2html: inline_app)
	case App(x, ys) if(M.mem(x, env) => 
		val (zs, e) = M.find(x, env);
		println( "inlining "+x+"@.");
		val envdash = List.fold_left2 (
				(envdash, (z, t), y) => M.add(z, y, envdash),
				M.empty,
				zs,
				ys
			);
			Alpha.g(envdash, e)
	case LetTuple(xts, y, e) => LetTuple(xts, y, g(env, e))
	case e => e
}

def f(e) = g(M.empty, e)
