open KNormal

def memi(x, env) = try {
	M.find(x, env) match {
		case Int(_) => true
		case _ => false
	}
} catch {
	case Not_found => false
}

def memf(x, env) = try {
	M.find(x, env) match {
		case Float(_) => true
		case _ => false
	}
} catch {
	case Not_found => false
}

def memt(x, env) = try {
	M.find(x, env) match {
		case Tuple(_) => true
		case _ => false
	}
} catch {
	case Not_found => false
}

def findi(x, env) = M.find(x, env) match {
	case Int(i) => i
	case _ => throw new Not_found
}

def findf(x, env) = M.find(x, env) match {
	case Float(d) => d 
	case _ => throw new Not_found
}
def findt(x, env) = M.find(x, env) match {
	case Tuple(ys) => ys
	case _ => throw new Not_found
}

// 定数畳み込みルーチン本体 (caml2html: constfold_g)
def g(env, e) = e match {
	case Var(x) if (memi(x, env)) => Int(findi(x, env))
	// case Var(x) if (memf(x, env)) => Float(findf(x, env))
	// case Var(x) if (memt(x, env)) => Tuple(findt(x, env))
	case Neg(x) if (memi(x, env)) => Int(-(findi(x, env)))
	case Add(x, y) if (memi(x, env) && memi(y, env)) => Int(findi(x, env) + findi(y, env)) (* 足し算のケース (caml2html: constfold_add) *)
	case Sub(x, y) if (memi(x, env) && memi(y, env)) => Int(findi(x, env) - findi(y, env))
	case FNeg(x) if (memf(x, env)) => Float(-.(findf(x, env)))
	case FAdd(x, y) if (memf(x, env) && memf(y, env)) => Float(findf(x, env) + findf(y, env))
	case FSub(x, y) if (memf(x, env) && memf(y, env)) => Float(findf(x, env) - findf(y, env))
	case FMul(x, y) if (memf(x, env) && memf(y, env)) => Float(findf(x, env) * findf(y, env))
	case FDiv(x, y) if (memf(x, env) && memf(y, env)) => Float(findf(x, env) / findf(y, env))
	case IfEq(x, y, e1, e2) if (memi(x, env) && memi(y, env)) => if (findi(x, env) == findi(y, env)) g(env, e1) else g(env, e2)
	case IfEq(x, y, e1, e2) if (memf(x, env) && memf(y, env)) => if (findf(x, env) == findf(y, env)) g(env, e1) else g(env, e2)
	case IfEq(x, y, e1, e2) => IfEq(x, y, g(env, e1), g(env, e2))
	case IfLE(x, y, e1, e2) if (memi(x, env) && memi(y, env)) => if (findi(x, env) <= findi(y, env)) g(env, e1) else g(env, e2)
	case IfLE(x, y, e1, e2) if (memf(x, env) && memf(y, env)) => if (findf(x, env) <= findf(y, env)) g(env, e1) else g(env, e2)
	case IfLE(x, y, e1, e2) => IfLE(x, y, g(env, e1), g(env, e2))
	case Let((x, t), e1, e2) => // letのケース (caml2html: constfold_let)
		val e1dash = g(env, e1);
		val e2dash = g(M.add(x, e1dash, env), e2);
		Let((x, t), e1dash, e2dash)
	case LetRec(Fundef(x, ys, e1), e2) => LetRec(Fundef(x, ys, g(env, e1)), g(env, e2))
	case LetTuple(xts, y, e) if (memt(y, env)) =>
		List.fold_left2(
			(edash, xt, z) => Let(xt, Var(z), edash),
			g(env, e),
			xts,
			findt(y, env)
		)
	case LetTuple(xts, y, e) => LetTuple(xts, y, g(env, e))
	case e => e
}

def f() = g(M.empty)
