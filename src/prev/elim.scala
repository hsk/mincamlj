open KNormal

// 副作用の有無 (caml2html: elim_effect)
def effect(e) = e match {
	case Let(_, e1, e2)     => effect(e1) || effect(e2)
	case IfEq(_, _, e1, e2) => effect(e1) || effect(e2)
	case IfLE(_, _, e1, e2) => effect(e1) || effect(e2)
	case LetRec(_, e)       => effect(e)
	case LetTuple(_, _, e)  => effect(e)
	case App(_) | Put(_) | ExtFunApp(_) => true
	case _ => false
}

// 不要定義削除ルーチン本体 (caml2html: elim_f)
def f(e) = e match {
	case IfEq(x, y, e1, e2) => IfEq(x, y, f(e1), f(e2))
	case IfLE(x, y, e1, e2) => IfLE(x, y, f(e1), f(e2))
	case Let((x, t), e1, e2) => // letの場合 (caml2html: elim_let)
		val e1dash = f(e1);
		val e2dash = f(e2);
		if (effect(e1dash) || S.mem(x, fv(e2dash)) ) {
			Let((x, t), e1dash, e2dash)
		} else {
			println("eliminating variable "+x+"@.") ;
			e2dash
		}
	case LetRec(Fundef((x, t),yts,e1), e2) => // let recの場合 (caml2html: elim_letrec)
		val e2dash = f(e2);
		if (S.mem(x,fv(e2dash))) {
			LetRec(Fundef((x, t), yts, f(e1)), e2dash)
		} else {
			println("eliminating function "+x+"@.");
			e2dash
		}
	case LetTuple(xts, y, e) =>
		val xs = xts.map(fst);
		val edash = f(e);
		val live = fv(edash);
		if (List.exists(x => S.mem(x, live), xs)) {
			LetTuple(xts, y, edash)
		} else {
			println("eliminating variables "+Id.pp_list(xs)+"@.");
			edash
		}
	case e => e
}
