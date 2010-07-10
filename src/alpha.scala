package mincaml;
import scala.collection.immutable.HashMap;

// rename identifiers to make them unique (alpha-conversion)

object Alpha extends KNormal {

	// 外部変数は変換しない
	def find(x:Id.T, env:Map[Id.T, Id.T]):Id.T = try {
		env(x)
	} catch {
		case _ => x
	}

	// α変換ルーチン本体 (caml2html: alpha_g)
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
		case Let((x, t), e1, e2) => // letのα変換 (caml2html: alpha_let)
			val xdash = Id.genid(x);// 新しい名前を作成
			Let((xdash, t), g(env, e1), g(env + (x -> xdash), e2))
		case Var(x) => Var(find(x, env))
		case LetRec(Fundef((x, t), yts, e1), e2) => // let recのα変換 (caml2html: alpha_letrec)
			// 関数名
			val env2 = env + (x -> Id.genid(x));// 新しい名前作成
			// 引数部
			val envdash = yts.foldLeft(env2){ case (e, (k, _)) => e + (k -> Id.genid(k)) }
			LetRec(
				Fundef(
					(find(x, env), t),
					// 生成した名前適応
					yts.map{ case (y, t) => (find(y, envdash), t) },
					// body部
					g(envdash, e1)
				),
				g(env, e2)
			)
		case App(x, ys) => App(find(x, env), ys.map{ find(_, env) })
		case Tuple(xs) => Tuple(xs.map{ find(_, env) })
		case LetTuple(xts, y, e) => // LetTupleのα変換 (caml2html: alpha_lettuple)
			val envdash = xts.foldLeft(env){ case (e, (k, _)) => e + (k -> Id.genid(k)) }
			LetTuple(
				xts.map{ case (x, t) => (find(x, envdash), t) },
				find(y, env),
				g(envdash, e)
			)
		case Get(x, y)        => Get(find(x, env), find(y, env))
		case Put(x, y, z)     => Put(find(x, env), find(y, env), find(z, env))
		case ExtArray(x)      => ExtArray(x)
		case ExtFunApp(x, ys) => ExtFunApp(x, ys.map{ find(_, env) } )
	}

	def f(e:KNormal.T):KNormal.T = g(Map[Id.T, Id.T](), e.asInstanceOf[T]).asInstanceOf[KNormal.T]
}
