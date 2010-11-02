/*
 * 定数畳み込み(constFold.scala)
 */

package mincaml;
import scala.collection.immutable._;

object ConstFold extends KNormal {

	def memi(x:Id.T, env:Map[Id.T, T]):Boolean = {
		try {
			env(x) match {
				case Int(_) => true
				case _      => false
			}
		} catch {
			case _ => false
		}
	}
	def memf(x:Id.T, env:Map[Id.T, T]):Boolean = {
		try {
			env(x) match {
				case Float(_) => true
				case _        => false
			}
		} catch {
			case _ => false
		}
	}
	
	def memt(x:Id.T, env:Map[Id.T, T]):Boolean = {
		try {
			env(x) match {
				case Tuple(_) => true
				case _        => false
			}
		} catch {
			case _ => false
		}
	}

	def findi(x:Id.T, env:Map[Id.T, T]):scala.Int = {
		env.get(x) match {
			case Some(Int(i)) => i
			case _ => throw new Exception()
		}
	}

	def findf(x:Id.T, env:Map[Id.T, T]):scala.Double = {
		env.get(x) match {
			case Some(Float(d)) => d 
			case _ => throw new Exception()
		}
	}

	def findt(x:Id.T, env:Map[Id.T, T]):List[Id.T] = {
		env.get(x) match {
			case Some(Tuple(ys)) => ys
			case _ => throw new Exception()
		}
	}

	/**
	 * 定数畳み込みルーチン本体
	 *
	 * ConstFold.gは、変数の名前から値への写像envと、式eとを受け取り、定数畳み込みを行って返します。
	 * たとえばx + yという式を受け取ったら、xとyの値が整数定数かどうか調べ、もしそうだったら直ちに計算して結果を返します。
	 * 逆にlet x = e in ...という変数定義を見つけたら、xからeへの対応をenvに追加します。
	 * 整数だけでなく、浮動小数や組についても同じです。
	 * @param env:Map[Id.T, T] 変数名から式の環境
	 * @param e:KNormal.T 式
	 * @return KNormal.T 最適化後の式
	 */
	def g(env:Map[Id.T,T], e:T):T = {
		e match {

			// 変数でxのint値が環境にあれば環境からxの値を取り出してたたみこむ
			case Var(x) if (memi(x, env)) => Int(findi(x, env))
			// case Var(x) if (memf(x, env)) => Float(findf(x, env))
			// case Var(x) if (memt(x, env)) => Tuple(findt(x, env))
			
			// 定数のマイナスならマイナス値を計算して返す
			case Neg(x) if (memi(x, env)) => Int(-(findi(x, env)))

			// 両方が定数なら足し算した結果を返す
			case Add(x, y) if (memi(x, env) && memi(y, env)) => Int(findi(x, env) + findi(y, env)) // 足し算のケース (caml2html: constfold_add)
			case Sub(x, y) if (memi(x, env) && memi(y, env)) => Int(findi(x, env) - findi(y, env))
			case FNeg(x) if (memf(x, env)) => Float(-findf(x, env))
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
				val e2dash = g(env + (x -> e1dash), e2);
				Let((x, t), e1dash, e2dash)
			case LetRec(Fundef(x, ys, e1), e2) => LetRec(Fundef(x, ys, g(env, e1)), g(env, e2))
			case LetTuple(xts, y, e) if (memt(y, env)) =>
				foldLeft2(
					g(env, e),
					xts,
					findt(y, env),
					(edash, xt, z) => Let(xt, Var(z), edash)
				)
			case LetTuple(xts, y, e) => LetTuple(xts, y, g(env, e))
			case e => e
		}
	}

	/*
	 * foldLeftの２引数バージョン
	 * todo: foldLeftの意味を調べる
	 */
	def foldLeft2(
		env:T,
		zs:List[(Id.T, Type.T)],
		ys:List[Id.T],
		f1:(T, (Id.T, Type.T), Id.T) => T
	):T = {
		(zs, ys) match {
			case (z::List(), y::List()) => f1(env, z, y)
			case (z::zs, y::ys) =>
				val env2:T = f1(env, z, y);
				foldLeft2(env2, zs, ys, f1)
			case _ => throw new Exception();
		}
	}

	/**
	 * 定数畳み込み(constFold.ml)
	 * 関数をインライン展開すると、たとえばlet x = 3 in let y = 7 in x + yにおけるx + yのように、
	 * すでに値がわかっている変数についての計算がよく出てきます。
	 * これを実際に計算して、コンパイル時に「10」のような定数に置き換えてしまう最適化処理が定数畳み込みです。
	 */
	def apply(e:KNormal.T):KNormal.T = {
		g(Map[Id.T, T](), e.asInstanceOf[T]).asInstanceOf[KNormal.T]
	}

}
