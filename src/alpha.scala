package mincaml;

// rename identifiers to make them unique (alpha-conversion)
import scala.collection.mutable.HashMap;

object KNormal {

	def find(x, env) = {
		try {
			M.find(x, env)
		} catch {
			case Not_found => x
		}
	}

	// α変換ルーチン本体 (caml2html: alpha_g)
	def g(env, e):T = {
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
			val xdash = Id.genid(x);
			Let((xdash, t), g(env, e1), g(M.add(x, xdash, env), e2))
		case Var(x) => Var(find(x, env))
		case LetRec(Fundef((x, t), yts, e1), e2) => // let recのα変換 (caml2html: alpha_letrec)
			val env = M.add(x, Id.genid(x), env);
			val ys = yts.map(fst);
			val envdash = M.add_list2(ys, ys.map(Id.genid), env);
			LetRec(
				Fundef(
					(find(x, env), t),
					yts.map(
						(y, t) => (find(y, envdash), t)
					),
					g(envdash, e1)
				),
				g(env, e2)
			)
		case App(x, ys) => App(find(x, env), ys.map(y => find(y, env)))
		case Tuple(xs) => Tuple(xs.map(x => find(x, env)))
		case LetTuple(xts, y, e) => // LetTupleのα変換 (caml2html: alpha_lettuple)
			val xs = xts.map(fst);
			val envdash = M.add_list2(xs, xs.map(Id.genid), env);
			LetTuple(
				xts.map(
					(x, t) => (find(x, envdash), t)
				),
				find(y, env),
				g(envdash, e)
			)
		case Get(x, y)        => Get(find(x, env), find(y, env))
		case Put(x, y, z)     => Put(find(x, env), find(y, env), find(z, env))
		case ExtArray(x)      => ExtArray(x)
		case ExtFunApp(x, ys) => ExtFunApp(x, ys.map(y => find(y, env)) )
	}

	def f(e:T):T = g(e, new HashMap[Any,Option[Type.T]])
}
