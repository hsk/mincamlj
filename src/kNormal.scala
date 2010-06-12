package mincaml;
import scala.collection.immutable._;

// give names to intermediate values (K-normalization)

// K正規化後の式 (caml2html: knormal_t)

class KNormal {
	sealed abstract class T();
	case class Unit() extends T;
	case class Int(a:scala.Int) extends T;
	case class Float(a:scala.Double) extends T;
	case class Neg(a:Id.T) extends T;
	case class Add(a:Id.T, b:Id.T) extends T;
	case class Sub(a:Id.T, b:Id.T) extends T;
	case class FNeg(a:Id.T) extends T;
	case class FAdd(a:Id.T, b:Id.T) extends T;
	case class FSub(a:Id.T, b:Id.T) extends T;
	case class FMul(a:Id.T, b:Id.T) extends T;
	case class FDiv(a:Id.T, b:Id.T) extends T;
	case class IfEq(a:Id.T, b:Id.T, c:T, d:T) extends T; // 比較 + 分岐 (caml2html: knormal_branch)
	case class IfLE(a:Id.T, b:Id.T, c:T, d:T) extends T;// 比較 + 分岐 
	case class Let(a:(Id.T, Type.T), b:T, c:T) extends T;
	case class Var(a:Id.T) extends T;
	case class LetRec(a:Fundef, b:T) extends T;
	case class App(a:Id.T, b:List[Id.T]) extends T;
	case class Tuple(a:List[Id.T]) extends T;
	case class LetTuple(a:List[(Id.T,Type.T)], b:Id.T, c:T) extends T;
	case class Get(a:Id.T, b:Id.T) extends T;
	case class Put(a:Id.T, b:Id.T, c:Id.T) extends T;
	case class ExtArray(a:Id.T) extends T;
	case class ExtFunApp(a:Id.T, b:List[Id.T]) extends T;
	case class Fundef(name:(Id.T, Type.T), args:List[(Id.T, Type.T)], body:T);
}

object KNormal extends KNormal {


	// 式に出現する（自由な）変数 (caml2html: knormal_fv)
/*
	def fv(e:T):S.t = e match {
		case Unit() | Int(_) | Float(_) | ExtArray(_) => S.empty()
		case Neg(x) => S.singleton(x)
		case FNeg(x) => S.singleton(x)
		case Add(x, y) => S.of_list(List(x, y))
		case Sub(x, y) => S.of_list(List(x, y))
		case FAdd(x, y) => S.of_list(List(x, y))
		case FSub(x, y) => S.of_list(List(x, y))
		case FMul(x, y) => S.of_list(List(x, y))
		case FDiv(x, y) => S.of_list(List(x, y))
		case IfEq(x, y, e1, e2) => S.add(x, S.add(y, S.union(fv(e1),fv(e2))))
		case IfLE(x, y, e1, e2) => S.add(x, S.add(y, S.union(fv(e1),fv(e2))))
		case Let((x, t), e1, e2) => S.union(fv(e1), S.remove(x, fv(e2)))
		case Var(x) => S.singleton(x)
		case LetRec(Fundef((x, t), yts, e1), e2) =>
			val zs = S.diff (fv(e1), S.of_list(yts.map(fst)));
			S.diff(S.union(zs, fv(e2)), S.singleton(x))
		case App(x, ys) => S.of_list(x :: ys)
		case Tuple(xs) => S.of_list(xs)
		case ExtFunApp(_, xs) => S.of_list(xs)
		case Put(x, y, z) => S.of_list(List(x,y,z))
		case Get(x, y) => S.of_list(List(x, y))
		case LetTuple(xs, y, e) => S.add(y, S.diff(fv(e), S.of_list(xs.map(fst))))
	}
*/

	// letを挿入する補助関数 (caml2html: knormal_insert)
	def insert_let(e1:(T, Any), k:(Id.T)=>(T, Type.T)):(T, Type.T) = e1 match {
		case (e,Var(x)) => val x2:Id.T = x; k(x2)
		case (e,t:Type.T) =>
			val x:Id.T = Id.gentmp(t)
			val (edash, tdash) = k(x)
			(Let((x, t), e, edash), tdash)
	}

	// K正規化ルーチン本体 (caml2html: knormal_g)
	def g(env:Map[Id.T,Type.T], e:Syntax.T):(T, Type.T) = e match {
		case Syntax.Unit() => (Unit(), Type.Unit())
		case Syntax.Bool(b) => (Int(if(b) 1 else 0), Type.Int()) // 論理値true, falseを整数1, 0に変換 (caml2html: knormal_bool)
		case Syntax.Int(i) => (Int(i), Type.Int())
		case Syntax.Float(d) => (Float(d), Type.Float())
		case Syntax.Not(e) => g(env, Syntax.If(e, Syntax.Bool(false), Syntax.Bool(true)))
		case Syntax.Neg(e) => insert_let(g(env, e), x => (Neg(x), Type.Int()))

		// 足し算のK正規化 (caml2html: knormal_add)
		case Syntax.Add(e1, e2) =>
			insert_let(
				g(env, e1),
				x => insert_let(
					g(env, e2),
					y => (Add(x, y), Type.Int())
				)
			)
		case Syntax.Sub(e1, e2) =>
			insert_let(
				g(env, e1),
				x => insert_let(
					g(env, e2),
					y => (Sub(x, y), Type.Int())
				)
			)
		case Syntax.FNeg(e) =>
			insert_let(
				g(env, e),
				x => (FNeg(x), Type.Float())
			)
		case Syntax.FAdd(e1, e2) =>
			insert_let(
				g(env, e1),
				x => insert_let(
					g(env, e2),
					y => (FAdd(x, y), Type.Float())
				)
			)
		case Syntax.FSub(e1, e2) =>
			insert_let(
				g(env, e1),
				x => insert_let(
					g(env, e2),
					y => (FSub(x, y), Type.Float())
				)
			)
		case Syntax.FMul(e1, e2) =>
			insert_let(
				g(env, e1),
				x => insert_let(
					g(env, e2),
					y => (FMul(x, y), Type.Float())
				)
			)
		case Syntax.FDiv(e1, e2) =>
			insert_let(
				g(env, e1),
				x => insert_let(
					g(env, e2),
					y => (FDiv(x, y), Type.Float())
				)
			)
		case cmp@(Syntax.Eq(_,_) | Syntax.LE(_,_))=>
			g(env, Syntax.If(cmp, Syntax.Bool(true), Syntax.Bool(false)))
		case Syntax.If(Syntax.Not(e1), e2, e3) => g(env, Syntax.If(e1, e3, e2)) // notによる分岐を変換 (caml2html: knormal_not)
		case Syntax.If(Syntax.Eq(e1, e2), e3, e4) =>
			insert_let(
				g(env, e1),
				x => insert_let(
					g(env, e2),
					y => {
						val (e3dash:T, t3:Type.T) = g(env, e3);
						val (e4dash:T, t4:Type.T) = g(env, e4);
						(IfEq(x, y, e3dash, e4dash), t3);
					}
				)
			)
		case Syntax.If(Syntax.LE(e1, e2), e3, e4) =>
			insert_let(
				g(env, e1),
				x => insert_let(
					g(env, e2),
					y => {
						val (e3dash, t3) = g(env, e3);
						val (e4dash, t4) = g(env, e4);
						(IfLE(x, y, e3dash, e4dash), t3);
					}
				)
			)
		case Syntax.If(e1, e2, e3) =>
			g(env, Syntax.If(Syntax.Eq(e1, Syntax.Bool(false)), e3, e2)) // 比較のない分岐を変換 (caml2html: knormal_if)
		case Syntax.Let((x, t), e1, e2) =>
			val (e1dash, t1) = g(env, e1);
			val (e2dash, t2) = { env + (x -> t); g(env, e2); }
			(Let((x, t), e1dash, e2dash), t2)
		case Syntax.Var(x) if(env.contains(x)) => (Var(x), env(x))
		case Syntax.Var(x) => // 外部配列の参照 (caml2html: knormal_extarray)
			Typing.extenv(x) match {
				case t@Type.Array(_) => (ExtArray(x), t)
				case _ => throw new Exception("external variable "+ x +" does not have an array type")
			}
		case Syntax.LetRec(Syntax.Fundef((x, t),yts,e1), e2) =>
			val envdash = env; envdash + (x -> t)
			val (e2dash, t2) = g(envdash, e2)
			val (e1dash, t1) = g (add_list(yts, envdash), e1)
			(LetRec(Fundef((x, t), yts, e1dash), e2dash), t2)
		case Syntax.App(Syntax.Var(f), e2s) if (!env.contains(f)) => // 外部関数の呼び出し (caml2html: knormal_extfunapp)
			Typing.extenv(f) match {
				case Type.Fun(_, t) =>
					def bind (xs:List[Id.T], e:List[Syntax.T]):(T, Type.T) = e match {// "xs" are identifiers for the arguments 
						case List() => (ExtFunApp(f, xs), t)
						case e2 :: e2s =>
//	def insert_let(e1:(T, Any), k:(Id.T)=>(T, Type.T)):(T, Type.T) = e1 match {

							insert_let(
								g(env, e2),
								x => bind (xs ::: List(x), e2s)
							)
					}
					bind(List[Id.T](), e2s) // left-to-right evaluation
				case _ => throw new Exception()
			}
		case Syntax.App(e1, e2s) =>
			g(env, e1) match {
				case g_e1@(_, Type.Fun(_, t)) =>
					insert_let(
						g_e1,
						f => {
						def bind (xs:List[Id.T], es:List[Syntax.T]):(T, Type.T) = es match {// "xs" are identifiers for the arguments 
								case List() => (App(f, xs), t)
								case e2 :: e2s =>
									insert_let(
										g(env, e2),
										x => bind(xs ::: List(x), e2s)
									)
							}
							bind(List[Id.T](), e2s)
						} // left-to-right evaluation
					)
				case _ => throw new Exception();
			}
		case Syntax.Tuple(es) =>

			def bind (xs:List[Id.T], ts:List[Type.T], es:List[Syntax.T]):(T, Type.T) = es match {// "xs" and "ts" are identifiers and types for the elements
			case List() => (Tuple(xs), Type.Tuple(ts))
			case e :: es =>
				val g_e@(_, t) = g(env, e);
				insert_let(
					g_e,
					x => bind(xs ::: List(x), ts ::: List(t), es)
				)
			}
			bind(List(), List(), es)
		case Syntax.LetTuple(xts, e1, e2) =>
			insert_let(
				g(env, e1),
				y => {
					val(e2dash, t2) = g(add_list(xts, env), e2);
					(LetTuple(xts, y, e2dash), t2)
				}
			)
		case Syntax.Array(e1, e2) =>
			insert_let(
				g(env, e1),
				x => {
					val g_e2@(_, t2:Type.T) = g(env, e2);

				  	insert_let(
						g_e2,
						y => {
							val l = t2 match {
							case Type.Float() => "create_float_array"
							case _         => "create_array"
							}
							(ExtFunApp(Id.T(l), List(x, y)), Type.Array(t2))
						}
					)
				}
			)
		case Syntax.Get(e1, e2) =>
			g(env, e1) match {
				case g_e1@(_, Type.Array(t)) =>
					insert_let(
						g_e1,
						x => insert_let(
							g(env, e2),
							y => (Get(x, y), t)
						)
					)
				case _ => throw new Exception()
			}
		case Syntax.Put(e1, e2, e3) =>
			insert_let(
				g(env, e1),
				x => insert_let(
					g(env, e2),
					y => insert_let(
						g(env, e3),
						z => (Put(x, y, z), Type.Unit())
					)
				)
			)
	}

	def f(e:Syntax.T):T = {
		val (a, b) = g(new HashMap[Id.T, Type.T], e)
		a
	}

	// add_list
	def add_list(xys:List[(Id.T, Type.T)], env:Map[Id.T,Type.T]):Map[Id.T,Type.T] = {
		xys.foldLeft(env) {
		case (x,(a, b)) => x + (a->b)
		}
	}

}
