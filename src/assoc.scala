/*
ネストしたletの簡約(assoc.ml)
 
式の形を見やすくするために、
let x = (let y = e1 in e2) in e3のようにネストしたletを、
let y = e1 in let x = e2 in e3のように平たくします。
これはMinCamlでコンパイルされたプログラムの性能に（直接は）影響しませんが、
コンパイラのデバッグや実験のときに、人間にとってわかりやすくするためです。
 
この簡約はAssoc.fで実装されています。
let x = e1 in e2という形の式があったら、まず再帰によりe1をe1'に、e2をe2'に簡約します。
そして、e1'がlet ... in eのようになっていたら、
最後のinの直後にlet x = e in e2'を挿入して、let ... in let x = e in e2'という式を返します。
ちょっとトリッキーですが、できてしまえば非常に簡単です（assoc.mlはわずか21行です）。
*/
/**
 * flatten let-bindings (just for prettier printing)
 */
package mincaml;

object Assoc extends KNormal {

	/**
	 * ネストしたletの簡約
	 */
	def g(e:T):T = e match {
		case IfEq(x, y, e1, e2) => IfEq(x, y, g(e1), g(e2))
		case IfLE(x, y, e1, e2) => IfLE(x, y, g(e1), g(e2))
		// letの場合
		case Let(xt, e1, e2) =>
			def insert(e1:T):T = e1 match {
				case Let(yt, e3, e4)     => Let(yt, e3, insert(e4))
				case LetRec(fundefs, e)  => LetRec(fundefs, insert(e))
				case LetTuple(yts, z, e) => LetTuple(yts, z, insert(e))
				case e                   => Let(xt, e, g(e2))
			}
			insert(g(e1))
		case LetRec(Fundef(xt, yts, e1), e2) => LetRec(Fundef(xt, yts, g(e1)), g(e2))
		case LetTuple(xts, y, e) => LetTuple(xts, y, g(e))
		case e => e
	}

	def apply(e:KNormal.T):KNormal.T = {
		g(e.asInstanceOf[T]).asInstanceOf[KNormal.T]
	}
}
