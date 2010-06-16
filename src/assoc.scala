// flatten let-bindings (just for prettier printing)
package mincaml;

object Assoc extends KNormal {

	// ネストしたletの簡約 (caml2html: assoc_f)
	def g(e:T):T = e match {
		case IfEq(x, y, e1, e2) => IfEq(x, y, g(e1), g(e2))
		case IfLE(x, y, e1, e2) => IfLE(x, y, g(e1), g(e2))

		// letの場合 (caml2html: assoc_let)
		case Let(xt, e1, e2) =>
			def insert(e1:T):T = e1 match {
				case Let(yt, e3, e4)     => Let(yt, e3, insert(e4))
				case LetRec(fundefs, e)  => LetRec(fundefs, insert(e))
				case LetTuple(yts, z, e) => LetTuple(yts, z, insert(e))
				case e                   => Let(xt, e, g(e2))
			}
			insert(g(e1))
		case LetRec(Fundef(xt,yts,e1), e2) => LetRec(Fundef(xt,yts,g(e1)), g(e2))
		case LetTuple(xts, y, e) => LetTuple(xts, y, g(e))
		case e => e
	}
	def f(e:KNormal.T):KNormal.T = g(e.asInstanceOf[T]).asInstanceOf[KNormal.T]
}
