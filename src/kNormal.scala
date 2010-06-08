package mincaml;
/* give names to intermediate values (K-normalization) */

/* K正規化後の式 (caml2html: knormal_t) */

object kNormal {

	sealed abstract class T();
	case class Unit() extends T;
	case class Int(a:Int) extends T;
	case class Float(a:Float) extends T;
	case class Neg(a:Id.T) extends T;
	case class Add(a:Id.T, b:Id.T) extends Bin;
	case class Sub(a:Id.T, b:Id.T) extends Bin;
	case class FNeg(a:Id.T) extends T;
	case class FAdd(a:Id.T, b:Id.T) extends Bin;
	case class FSub(a:Id.T, b:Id.T) extends Bin;
	case class FMul(a:Id.T, b:Id.T) extends Bin;
	case class FDiv(a:Id.T, b:Id.T) extends Bin;
	case class IfEq(a:Id.T, b:Id.T, c:T, d:T) extends T; // 比較 + 分岐 (caml2html: knormal_branch)
	case class IfLE(a:Id.T, b:Id.T, c:T, d:T) extends T;// 比較 + 分岐 
	case class Let(a:(Id.T, Type.T), b:T, c:T) extends T;
	case class Var(a:Id.T) extends T;
	case class LetRec(a:Fundef, b:t) extends T;
	case class App(a:Id.T, b:List[Id.T]) extends T;
	case class Tuple(a:List[Id.T]) extends T;
	case class LetTuple(a:List[(Id.T,Type.T)], b:Id.T, c:T) extends T;
	case class Get(a:Id.T, b:Id.T) extends Bin;
	case class Put(a:Id.T, b:Id.T, c:Id.T) extends T;
	case class ExtArray(a:Id.T) extends T;
	case class ExtFunApp(a:Id.T, b:List[Id.T]) extends T;
	case class Fundef(name:(Id.T, Type.T), args:List[(Id.T, Type.T)], body:t);

	/* 式に出現する（自由な）変数 (caml2html: knormal_fv) */
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

	/* letを挿入する補助関数 (caml2html: knormal_insert) */
	def insert_let(e1:(T, Type.T), k:(Id.T)=>(T, T)):(T, T) = e1 match {
		case (e,Var(x)) => k(x)
		case (e,t) =>
			val x = Id.Id.gentmp(t)
			val (edash, tdash) = k(x)
			(Let((x, t), e, edash), tdash)
	}

	/* K正規化ルーチン本体 (caml2html: knormal_g) */
	def g(env:HashMap[Any,Option[Type.T]])(e):(T,Type.T) = e match {
		case syntax.Unit() => (Unit, Type.Unit)
		case syntax.Bool(b) => (Int(if(b) 1 else 0), Type.Int) // 論理値true, falseを整数1, 0に変換 (caml2html: knormal_bool)
		case syntax.Int(i) => (Int(i), Type.Int)
		case syntax.Float(d) => (Float(d), Type.Float)
		case syntax.Not(e) => g(env, syntax.If(e, syntax.Bool(false), syntax.Bool(true)))
		case syntax.Neg(e) => insert_let(g(env, e), x => (Neg(x), Type.Int))

		// 足し算のK正規化 (caml2html: knormal_add)
		case syntax.Add(e1, e2) =>
			insert_let(
				g(env, e1),
				x => insert_let(
					g(env, e2),
					y => (Add(x, y), Type.Int)
				)
			)
		case syntax.Sub(e1, e2) =>
			insert_let(
				g(env, e1),
				x => insert_let(
					g(env, e2),
					y => (Sub(x, y), Type.Int)
				)
			)
		case syntax.FNeg(e) =>
			insert_let(
				g(env, e),
				x => (FNeg(x), Type.Float)
			)
		case syntax.FAdd(e1, e2) =>
			insert_let(
				g(env, e1),
				x => insert_let(
					g(env, e2),
					y => (FAdd(x, y), Type.Float)
				)
			)
		case syntax.FSub(e1, e2) =>
			insert_let(
				g(env, e1),
				x => insert_let(
					g(env, e2),
					y => (FSub(x, y), Type.Float)
				)
			)
		case syntax.FMul(e1, e2) =>
			insert_let(
				g(env, e1),
				x => insert_let(
					g(env, e2),
					y => (FMul(x, y), Type.Float)
				)
			)
		case syntax.FDiv(e1, e2) =>
			insert_let(
				g(env, e1),
				x => insert_let(
					g(env, e2),
					y => (FDiv(x, y), Type.Float)
				)
			)
		case cmp@(syntax.Eq(_) | yntax.LE(_))=>
			g(env, syntax.If(cmp, syntax.Bool(true), syntax.Bool(false)))
		case syntax.If(syntax.Not(e1), e2, e3) => g(env, syntax.If(e1, e3, e2)) // notによる分岐を変換 (caml2html: knormal_not)
		case syntax.If(syntax.Eq(e1, e2), e3, e4) =>
			insert_let(
				g(env, e1),
				x => insert_let(
					g(env, e2),
					y => {
						val (e3dash, t3) = g(env, e3);
						val (e4dash, t4) = g(env, e4);
						(IfEq(x, y, e3, e4), t3);
					}
				)
			)
		case syntax.If(syntax.LE(e1, e2), e3, e4) =>
			insert_let(
				g(env, e1)
				x => insert_let(
					g(env, e2),
					y => {
						val (e3dash, t3) = g(env, e3);
						val (e4dash, t4) = g(env, e4);
						(IfLE(x, y, e3dash, e4dash), t3);
					}
				)
			)
		case syntax.If(e1, e2, e3) =>
			g(env, syntax.If(syntax.Eq(e1, syntax.Bool(false)), e3, e2) // 比較のない分岐を変換 (caml2html: knormal_if)
		case syntax.Let((x, t), e1, e2) =>
			val (e1dash, t1) = g(env, e1);
			val (e2dash, t2) = g(M.add(x, t, env), e2);
			(Let((x, t), e1dash, e2dash), t2)
		case syntax.Var(x) if(M.mem(x, env)) => (Var(x), M.find(x,env))
		case syntax.Var(x) => // 外部配列の参照 (caml2html: knormal_extarray)
			M.find(x, !Typing.extenv) match {
				case t@Type.Array(_) => (ExtArray(x), t)
				case _ => throw new Exception("external variable "+ x +" does not have an array type")
			}
		case syntax.LetRec(syntax.Fundef((x, t),yts,e1), e2) =>
			val envdash = M.add(x, t, env)
			val (e2dash, t2) = g(envdash, e2)
			val (e1dash, t1) = g (M.add_list(yts, envdash), e1)
			(LetRec(Fundef((x, t), yts, e1dash), e2dash), t2)
		case syntax.App(syntax.Var(f), e2s) when not (M.mem f env) => // 外部関数の呼び出し (caml2html: knormal_extfunapp)
			M.find(f, !Typing.extenv) match {
				case Type.Fun(_, t) =>
					val bind (xs,e) => e match {// "xs" are identifiers for the arguments 
						case List() => (ExtFunApp(f, xs), t)
						case e2 :: e2s =>
							insert_let(
								g(env, e2),
								x => bind (xs ::: List(x), e2s)
							)
					}
					bind(List(), e2s) // left-to-right evaluation
				case _ => assert false
			}
		case syntax.App(e1, e2s) =>
			g(env, e1) match {
				case g_e1@(_, Type.Fun(_, t)) =>
					insert_let(
						g_e1,
						f => {
							val bind = (xs, es) => { // "xs" are identifiers for the arguments 
								case List() => (App(f, xs), t)
								case e2 :: e2s =>
									insert_let(
										g(env, e2),
										x => bind(xs ::: List(x), e2s)
									)
							}
							bind(List(), e2s)
						} // left-to-right evaluation
					)
				case _ => assert(false)
			}
		case syntax.Tuple(es) =>
			val bind = (xs, ts, es) => es match { // "xs" and "ts" are identifiers and types for the elements
			case List() => (Tuple(xs), Type.Tuple(ts))
			case e :: es =>
			    val g_e@(_, t) = g(env, e);
			    insert_let(
					g_e,
					x => bind(xs ::: List(x), ts ::: List(t), es)
				)
			}
			bind(List(), List(), es)
		case syntax.LetTuple(xts, e1, e2) =>
			insert_let(
				g(env, e1),
				y => {
					val(e2dash, t2) = g((M.add_list(xts, env), e2);
					(LetTuple(xts, y, e2dash), t2)
				}
			)
		case syntax.Array(e1, e2) =>
			insert_let(
				g(env, e1),
				x => {
					val g_e2@(_, t2) = g(env, e2);
				  	insert_let(
						g_e2,
						y => {
							val l = t2 match {
							case Type.Float => "create_float_array"
							case _         => "create_array"
							}
							(ExtFunApp(l, List(x, y)), Type.Array(t2))
						}
					)
				}
			)
		case syntax.Get(e1, e2) =>
			g(env, e1) match {
				case g_e1@(_, Type.Array(t)) =>
					insert_let(
						g_e1,
						x => insert_let(
							g(env, e2),
							y => (Get(x, y), t)
						)
					)
				case _ => assert(false)
			}
		case syntax.Put(e1, e2, e3) =>
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

	def f(e:syntax.T):T = {
		val (a,b) = g(new HashMap[Any,Option[Type.T]], e)
		a
	}
}
