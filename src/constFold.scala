package mincaml;
import scala.collection.immutable._;

object ConstFold extends KNormal {

	def memi(x:Id.T, env:Map[Id.T, T]):Boolean = try {
		env(x) match {
			case Int(_) => true
			case _      => false
		}
	} catch {
		case _ => false
	}

	def memf(x:Id.T, env:Map[Id.T, T]):Boolean = try {
		env(x) match {
			case Float(_) => true
			case _        => false
		}
	} catch {
		case _ => false
	}

	def memt(x:Id.T, env:Map[Id.T, T]):Boolean = try {
		env(x) match {
			case Tuple(_) => true
			case _        => false
		}
	} catch {
		case _ => false
	}

	def findi(x:Id.T, env:Map[Id.T, T]):scala.Int = env.get(x) match {
		case Some(Int(i)) => i
		case _ => throw new Exception()
	}

	def findf(x:Id.T, env:Map[Id.T, T]):scala.Double = env.get(x) match {
		case Some(Float(d)) => d 
		case _ => throw new Exception()
	}
	def findt(x:Id.T, env:Map[Id.T, T]):List[Id.T] = env.get(x) match {
		case Some(Tuple(ys)) => ys
		case _ => throw new Exception()
	}

	// 定数畳み込みルーチン本体 (caml2html: constfold_g)
	def g(env:Map[Id.T,T], e:T):T = e match {
		case Var(x) if (memi(x, env)) => Int(findi(x, env))
		// case Var(x) if (memf(x, env)) => Float(findf(x, env))
		// case Var(x) if (memt(x, env)) => Tuple(findt(x, env))
		case Neg(x) if (memi(x, env)) => Int(-(findi(x, env)))
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

	def foldLeft2(
		env:T,
		zs:List[(Id.T, Type.T)],
		ys:List[Id.T],
		f1:(T, (Id.T, Type.T), Id.T) => T
	):T = (zs, ys) match {
		case (z::List(), y::List()) => f1(env, z, y)
		case (z::zs, y::ys) =>
			val env2:T = f1(env, z, y);
			foldLeft2(env2, zs, ys, f1)
		case _ => throw new Exception();
	}

	def f(e:KNormal.T):KNormal.T = g(Map[Id.T, T](), e.asInstanceOf[T]).asInstanceOf[KNormal.T]
}
