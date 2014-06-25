package mincaml;
import scala.collection.immutable.HashMap;
/*
β簡約(beta.ml)
 
 

*/
/**
 * β簡約
 */
object Beta extends KNormal {

	/**
	 * 置換のための関数
	 *
     * K正規形では、ありとあらゆる式に変数が出てくるので置換する
     * @param x:Id.T 変数
     * @param env:Map[Id.T, Id.T] ある変数からそれに等しい変数への写像
	 */
	def find(x:Id.T, env:Map[Id.T, Id.T]):Id.T = {
		try {
			env(x)
		} catch {
			case _ => x
		}
	}

	/**
	 * β簡約ルーチン本体
	 *
	 * ある変数からそれに等しい変数への写像envと、式eとを受け取り、eをβ簡約した式を返します。
	 * やはりポイントはlet x = e1 in e2の場合で、e1をβ簡約した結果が変数yだったら、
	 * xからyへの対応をenvに追加して、e2をβ簡約します。
	 * そして、変数xが出てきたら、envを引いて、変数yに置き換えてしまいます。
	 *
	 * @param env:Map[Id.T, Id.T] ある変数からそれに等しい変数への写像
	 * @param e:T 式
	 * @return T β簡約した式
	 */
	def g(env:Map[Id.T, Id.T], e:T):T = e match {
		case Unit() => Unit()
		case Int(i) => Int(i)
		case Float(d) => Float(d)
		case Neg(x) => Neg(find(x, env))
		case Add(x, y) => Add(find(x, env), find(y, env))
		case Sub(x, y) => Sub(find(x, env), find(y, env))
		case FNeg(x) => FNeg(find(x, env))
		case FAdd(x, y) => FAdd(find(x, env), find(y, env))
		case FSub(x, y) => FSub(find(x, env), find(y, env))
		case FMul(x, y) => FMul(find(x, env), find(y, env))
		case FDiv(x, y) => FDiv(find(x, env), find(y, env))
		case IfEq(x, y, e1, e2) => IfEq(find(x, env), find(y, env), g(env, e1), g(env, e2))
		case IfLE(x, y, e1, e2) => IfLE(find(x, env), find(y, env), g(env, e1), g(env, e2))
		case Let((x, t), e1, e2) => // letのβ簡約 (caml2html: beta_let)
			g(env, e1) match {
			case Var(y) =>
				println("beta-reducing "+x+" = "+y+"@.");
				g(env + (x -> y), e2)
			case e1dash =>
				val e2dash = g(env, e2);
				Let((x, t), e1dash, e2dash)
			}
		case LetRec(Fundef(xt, yts, e1), e2) =>
			LetRec(Fundef(xt, yts, g(env, e1)), g(env, e2))
		case Var(x) => Var(find(x, env)) // 変数を置換 (caml2html: beta_var)
		case Tuple(xs) => Tuple(xs.map{ find(_, env) })
		case LetTuple(xts, y, e) => LetTuple(xts, find(y, env), g(env, e))
		case Get(x, y) => Get(find(x, env), find(y, env))
		case Put(x, y, z) => Put(find(x, env), find(y, env), find(z, env))
		case App(g, xs) => App(find(g, env), xs.map{ find(_, env) })
		case ExtArray(x) => ExtArray(x)
		case ExtFunApp(x, ys) => ExtFunApp(x, ys.map{ find(_, env) })
	}

	/**
	 * 例)
	 * let x = y in x + y ではxとyが等しいのでx + yをy + yと置き換える。
	 *
	 * このような変換をK正規形のβ簡約といいます
	 *（λ計算という理論でもβ簡約という言葉がありますが、それの特殊な場合になっています）。
	 * 普通のプログラムだと、あまり必要な処理でもないのですが、他の最適化等をした後のプログラムでは効果がある場合もあります。
	 */
	def apply(e:KNormal.T):KNormal.T = {
		g(Map[Id.T, Id.T](), e.asInstanceOf[Beta.T]).asInstanceOf[KNormal.T]
	}
}
