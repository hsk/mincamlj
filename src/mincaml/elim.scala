/*
不要定義削除(elim.ml)
 
定数畳み込みを行うと、let x = 3 in let y = 7 in 10におけるxやyのように、
使われない変数定義や関数定義が出てきます。
MinCamlでは、これをElim.fで取り除きます。
 
一般に、e1に副作用がなく、x がe2に出現していなければ、
let x = e1 in e2という式を単なるe2に変換することができます。
この「副作用がない」という条件を実装したのがElim.effect、
「変数が式に出現する」という条件を実装したのがKNormal.fvです。
ただし、副作用が本当にあるかどうかは決定不能なので、
「配列への書き込みか、関数呼び出しがあったら副作用がある」と判定しています。
 
ちなみにKNormal.fvという名前は自由変数(free variable)という用語に由来します。
たとえばlet x = 3 in x + yという式には二つの変数x, yがありますが、
x は式の中で整数3に定義（束縛）されているので束縛変数といわれ、
yは束縛されていないので自由変数といいます。
*/
package mincaml;
import scala.collection.immutable._;

/**
 * 不要定義削除
 */
object Elim extends KNormal {

	/**
	 * 副作用の有無
	 */
	def effect(e:T):Boolean = {
		e match {
			case Let(_, e1, e2)     => effect(e1) || effect(e2)
			case IfEq(_, _, e1, e2) => effect(e1) || effect(e2)
			case IfLE(_, _, e1, e2) => effect(e1) || effect(e2)
			case LetRec(_, e)       => effect(e)
			case LetTuple(_, _, e)  => effect(e)
			case App(_, _) | Put(_, _, _) | ExtFunApp(_, _) => true
			case _ => false
		}
	}

	/**
	 * 不要定義削除ルーチン本体
	 */
	def g(e:T):T = {
		e match {
			case IfEq(x, y, e1, e2) => IfEq(x, y, g(e1), g(e2))
			case IfLE(x, y, e1, e2) => IfLE(x, y, g(e1), g(e2))
			case Let((x, t), e1, e2) => // letの場合 (caml2html: elim_let)
				val e1dash = g(e1)
				val e2dash = g(e2)
				if (effect(e1dash) || fv(e2dash).contains(x)) {
					Let((x, t), e1dash, e2dash)
				} else {
					println("eliminating variable "+x+"@.")
					e2dash
				}
			case LetRec(Fundef((x, t), yts, e1), e2) => // let recの場合 (caml2html: elim_letrec)
				val e2dash = g(e2);
				if (fv(e2dash).contains(x)) {
					LetRec(Fundef((x, t), yts, g(e1)), e2dash)
				} else {
					println("eliminating function "+x+"@.")
					e2dash
				}
			case LetTuple(xts, y, e) =>
				val xs = xts.map{ _._1 }
				val edash = g(e)
				val live = fv(edash)
				if (xs.exists{ live.contains(_) }) {
					LetTuple(xts, y, edash)
				} else {
					println("eliminating variables "+Id.pp_list(xs)+"@.")
					edash
				}
			case e => e
		}
	}

	def apply(e:KNormal.T):KNormal.T = {
		g(e.asInstanceOf[T]).asInstanceOf[KNormal.T]
	}
}
