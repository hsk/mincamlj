open X86Asm

// 命令列の13bit即値最適化 (caml2html: simm13_g)
def g(env, e) = e match {
	case Ans(exp) => Ans(gdash(env, exp))
	case Let((x, t), Set(i), e) if ((-4096 <= i) && (i < 4096)) =>
		// println("found simm13 " + x + " = " + i + "@.")
		val edash = g(M.add(x, i, env), e);
		if (List.mem(x, fv(edash))) {
			Let((x, t), Set(i), edash)
		} else {
			// println("erased redundant Set to "+x+"@.");
			edash
		}
	case Let(xt, SLL(y, C(i)), e) if (M.mem(y, env)) => // for array access
		// println("erased redundant SLL on " + x + "@.");
		g(env, Let(xt, Set(M.find(y, env), lsl(i)), e))
	case Let(xt, exp, e) => Let(xt, gdash(env, exp), g(env, e))
case Forget(x, e) => Forget(x, g(env, e))
}

// 各命令の13bit即値最適化 (caml2html: simm13_gprime)
def gdash(env, e) = e match {
	case Add(x, V(y)) if (M.mem(y, env)) => Add(x, C(M.find(y, env)))
	case Add(x, V(y)) if (M.mem(x, env)) => Add(y, C(M.find(x, env)))
	case Sub(x, V(y)) if (M.mem(y, env)) => Sub(x, C(M.find(y, env)))
	case SLL(x, V(y)) if (M.mem(y, env)) => SLL(x, C(M.find(y, env)))
	case Ld(x, V(y)) if (M.mem(y, env)) => Ld(x, C(M.find(y, env)))
	case St(x, y, V(z)) if (M.mem(z, env)) => St(x, y, C(M.find(z, env)))
	case LdDF(x, V(y)) if (M.mem(y, env)) => LdDF(x, C(M.find(y, env)))
	case StDF(x, y, V(z)) if (M.mem(z, env)) => StDF(x, y, C(M.find(z, env)))
	case IfEq(x, V(y), e1, e2) if (M.mem(y, env)) => IfEq(x, C(M.find(y, env)), g(env, e1), g(env, e2))
	case IfLE(x, V(y), e1, e2) if (M.mem(y, env)) => IfLE(x, C(M.find(y, env)), g(env, e1), g(env, e2))
	case IfGE(x, V(y), e1, e2) if (M.mem(y, env)) => IfGE(x, C(M.find(y, env)), g(env, e1), g(env, e2))
	case IfEq(x, V(y), e1, e2) if (M.mem(x, env)) => IfEq(y, C(M.find(x, env)), g(env, e1), g(env, e2))
	case IfLE(x, V(y), e1, e2) if (M.mem(x, env)) => IfGE(y, C(M.find(x, env)), g(env, e1), g(env, e2))
	case IfGE(x, V(y), e1, e2) if (M.mem(x, env)) => IfLE(y, C(M.find(x, env)), g(env, e1), g(env, e2))
	case IfEq(x, ydash, e1, e2) => IfEq(x, ydash, g(env, e1), g(env, e2))
	case IfLE(x, ydash, e1, e2) => IfLE(x, ydash, g(env, e1), g(env, e2))
	case IfGE(x, ydash, e1, e2) => IfGE(x, ydash, g(env, e1), g(env, e2))
	case IfFEq(x, y, e1, e2) => IfFEq(x, y, g(env, e1), g(env, e2))
	case IfFLE(x, y, e1, e2) => IfFLE(x, y, g(env, e1), g(env, e2))
	case e => e
}

// トップレベル関数の13bit即値最適化
def h(Fundef(l, xs, ys, e, t)) = Fundef(l, xs, ys, g(M.empty(), e), t)

// プログラム全体の13bit即値最適化
def f (Prog(data, fundefs, e)) = 
  Prog(data, fundefs.map(h) , g(M.empty(), e))
