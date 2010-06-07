// flatten let-bindings (just for prettier printing)

open KNormal

// ネストしたletの簡約 (caml2html: assoc_f)
def f(e) = e match {
	case IfEq(x, y, e1, e2) => IfEq(x, y, f e1, f e2)
	case IfLE(x, y, e1, e2) => IfLE(x, y, f e1, f e2)

	// letの場合 (caml2html: assoc_let)
	case Let(xt, e1, e2) =>
		val insert = e1 => e1 match {
			case Let(yt, e3, e4)     => Let(yt, e3, insert(e4))
			case LetRec(fundefs, e)  => LetRec(fundefs, insert(e))
			case LetTuple(yts, z, e) => LetTuple(yts, z, insert(e))
			case e                   => Let(xt, e, f(e2))
		}
		insert(f(e1))
	case LetRec(Fundef(xt,yts,e1), e2) => LetRec(Fundef(xt,yts,f(e1), f(e2))
	case LetTuple(xts, y, e) => LetTuple(xts, y, f e)
	case e => e
}
