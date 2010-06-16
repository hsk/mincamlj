package mincaml;
import scala.collection.immutable._;


object Typing extends Syntax {

	case class Unify(a:Type.T, b:Type.T) extends Exception
	case class Error(a:T, b:Type.T, c:Type.T) extends Exception
	case class Invalid() extends Exception

	// type inference/reconstruction
	var extenv = Map[Id.T, Type.T]()

	// for pretty printing (and type normalization) 
	// 型変数を中身でおきかえる関数
	def deref_typ(x:Type.T):Type.T = x match {
		case Type.Fun(t1s, t2) => Type.Fun(t1s.map(deref_typ), deref_typ(t2))
		case Type.Tuple(ts) => Type.Tuple(ts.map(deref_typ))
		case Type.Array(t) => Type.Array(deref_typ(t))
		case r@Type.Var(None) =>
			println( "uninstantiated type variable detected; assuming int"+r+".")
			r.a = Some(Type.Int())
			Type.Int()
		case r@Type.Var(Some(t)) =>
			val t1 = deref_typ(t)
			r.a = Some(t1)
			t1
		case t => t
	}

	def deref_id_typ(a:(Id.T, Type.T)):(Id.T, Type.T) = a match {
		case (x, t) => (x, deref_typ(t))
	}

	def deref_term(e:T):T = e match {
		case Not(e) => Not(deref_term(e))
		case Neg(e) => Neg(deref_term(e))
		case Add(e1, e2) => Add(deref_term(e1), deref_term(e2))
		case Sub(e1, e2) => Sub(deref_term(e1), deref_term(e2))
		case Eq(e1, e2) => Eq(deref_term(e1), deref_term(e2))
		case LE(e1, e2) => LE(deref_term(e1), deref_term(e2))
		case FNeg(e) => FNeg(deref_term(e))
		case FAdd(e1, e2) => FAdd(deref_term(e1), deref_term(e2))
		case FSub(e1, e2) => FSub(deref_term(e1), deref_term(e2))
		case FMul(e1, e2) => FMul(deref_term(e1), deref_term(e2))
		case FDiv(e1, e2) => FDiv(deref_term(e1), deref_term(e2))
		case If(e1, e2, e3) => If(deref_term(e1), deref_term(e2), deref_term(e3))
		case Let(xt, e1, e2) => Let( deref_id_typ (xt), deref_term(e1), deref_term(e2))
		case LetRec(Fundef(xt,yts, e1), e2) =>
			LetRec(
				Fundef(
					deref_id_typ(xt),
					yts.map(deref_id_typ),
					deref_term(e1)
				),
				deref_term(e2)
			)
		case App(e, es) => App(deref_term(e), es.map(deref_term))
		case Tuple(es) => Tuple(es.map(deref_term))
		case LetTuple(xts, e1, e2) => LetTuple(xts.map(deref_id_typ), deref_term(e1), deref_term(e2))
		case Array(e1, e2) => Array(deref_term(e1), deref_term(e2))
		case Get(e1, e2) => Get(deref_term(e1), deref_term(e2))
		case Put(e1, e2, e3) => Put(deref_term(e1), deref_term(e2), deref_term(e3))
		case e => e
	}

	// 頭［心］に浮かぶ、思い付く、気付く
	def occur(r1:Option[Type.T], r2:Type.T):Boolean = {
		r2 match { // occur check
		case Type.Fun(t2s/*:List[Type.T]*/, t2:Type.T)
		                                => t2s.exists(occur(r1,_)) || occur(r1, t2)
		case Type.Tuple(t2s)            => t2s.exists(occur(r1,_))
		case Type.Array(t2)             => occur(r1, t2)
		case Type.Var(None)             => false
		case Type.Var(r2) if (r1 == r2) => true
		case Type.Var(Some(t2))         => occur(r1, t2)
		case _ => false
		}
	}

	def iter2(
		f:(Type.T, Type.T) => scala.Unit,
		x:List[Type.T],
		y:List[Type.T]
	):scala.Unit = (x, y) match {
		case (a::as, b::bs) => f(a, b); iter2(f, as, bs)
		case (List(), List()) =>
		case a => throw new Invalid()
	}

	// 型が合うように、型変数への代入をする
	def unify(t1:Type.T, t2:Type.T) {
		(t1, t2) match {
		case (Type.Unit(), Type.Unit()) | (Type.Bool(), Type.Bool()) | (Type.Int(), Type.Int()) | (Type.Float(), Type.Float()) =>
		case (Type.Fun(t1s, t11), Type.Fun(t2s, t21)) =>
			try {
				iter2(unify, t1s, t2s)
			} catch {
				case Invalid() => throw Unify(t1, t2);
			}
			unify(t11, t21)
		case (Type.Tuple(t1s), Type.Tuple(t2s)) =>
			try {
				iter2(unify, t1s, t2s)
			} catch {
				case Invalid() => throw Unify(t1, t2)
			}
		case (Type.Array(t1), Type.Array(t2)) => unify(t1, t2)
		case (Type.Var(r1), Type.Var(r2)) if(r1 == r2) =>
		case (Type.Var(Some(t1dash)), _) => unify(t1dash, t2)
		case (_, Type.Var(Some(t2dash))) => unify(t1, t2dash)
		case (r@Type.Var(r1@None), _) => // 一方が未定義の型変数の場合
			if (occur(r1, t2)) {// 両方未定義ならエラー
				throw Unify(t1, t2);
			}
			r.a = Some(t2)
		case (_, r@Type.Var(r2@None)) =>
			if (occur (r2, t1)) {// 両方未定義ならエラー
				throw new Unify(t1, t2)
			}
			r.a = Some(t1)
		case (_, _) => throw new Unify(t1, t2)
		}
	}

	// 型推論ルーチン
	def g(env:Map[Id.T, Type.T], e:T):Type.T = try {
		def gint(e1:T,e2:T):Type.T = {
			unify(Type.Int(), g(env, e1))
			unify(Type.Int(), g(env, e2))
			Type.Int()
		}
		def gfloat(e1:T, e2:T):Type.T = {
			unify(Type.Float(), g(env, e1))
			unify(Type.Float(), g(env, e2))
			Type.Float()
		}
		def gbool(e1:T, e2:T):Type.T = {
			unify(g(env, e1), g(env, e2))
			Type.Bool()
		}
		e match {
			case Unit() => Type.Unit()
			case Bool(_) => Type.Bool()
			case Int(_) => Type.Int()
			case Float(_) => Type.Float()
			case Not(e) => unify(Type.Bool(), g(env, e)); Type.Bool()
			case Neg(e) => unify(Type.Int(), g(env, e)); Type.Int()
			case Add(e1, e2) => gint(e1, e2)
			case Sub(e1, e2) => gint(e1, e2) // 足し算（と引き算）の型推論
			case FNeg(e) => unify(Type.Float(), g(env, e)); Type.Float()
			case FAdd(e1, e2) => gfloat(e1, e2)
			case FSub(e1, e2) => gfloat(e1, e2)
			case FMul(e1, e2) => gfloat(e1, e2)
			case FDiv(e1, e2) => gfloat(e1, e2)
			case Eq(e1, e2) => gbool(e1, e2)
			case LE(e1, e2) => gbool(e1, e2)
			case If(e1, e2, e3) =>
				unify(g(env, e1), Type.Bool())
				val t2 = g(env, e2)
				val t3 = g(env, e3)
				unify(t2, t3)
				t2
			case Let((x, t), e1, e2) => // letの型推論
				unify(t, g(env, e1));
				//print(e + " env="+env+ " x="+x+ " t = "+ t);
				g(env + (x -> t), e2)
			case Var(x) if (env.get(x) != None) => env(x) // 変数の型推論
			case Var(x) if (extenv.get(x) != None) => extenv(x)
			case a@Var(x) => // 外部変数の型推論
				val t = Type.gentyp()
				println("free variable "+ x + " assumed as external "+a+"."+t)
				extenv = extenv + (x -> t)
				t
			case LetRec(Fundef((x, t), yts, e1), e2) => // let recの型推論
				val env2 = env + (x -> t)
				unify(t, Type.Fun(
					yts.map{_._2}, g(env2 ++ yts, e1)
				))
				g(env2, e2)
			case App(e, es) => // 関数適用の型推論
				val t = Type.gentyp()
				val a = es.map{g(env,_)};
				val b = g(env, e);
				unify(b, Type.Fun(a, t))
				t
			case Tuple(es) => Type.Tuple(es.map{g(env, _)})
			case LetTuple(xts, e1, e2) =>
				unify (Type.Tuple(xts.map{_._2}), g(env, e1))
				g(env ++ xts, e2)
			case Array(e1, e2) => // must be a primitive for "polymorphic" typing
				unify(g(env, e1), Type.Int())
				Type.Array(g(env, e2))
			case Get(e1, e2) =>
				val t = Type.gentyp()
				unify(Type.Array(t), g(env, e1))
				unify(Type.Int(), g(env, e2))
				t
			case Put(e1, e2, e3) =>
				val t = g(env, e3)
				unify(Type.Array(t), g(env, e1))
				unify(Type.Int(), g(env, e2))
				Type.Unit()
		}
	} catch {
		case a@Unify(t1, t2) =>
			a.printStackTrace();
			println(t1, t2);
			throw new Error(deref_term(e), deref_typ(t1), deref_typ(t2))
	}

	def f1(e:T):T = try {
		extenv = Map[Id.T, Type.T]()
/*		deref_typ(g(Map[Id.T, Type.T](), e)) match {
			case Type.Unit() => Unit
			case a => println("warning: final result does not have type unit"+a+"."); Unit
		}*/
		try {
			unify(Type.Unit(), g(Map[Id.T, Type.T](), e))
		} catch {
			case a@Unify(_, _) =>
				a.printStackTrace();
				throw new Exception("top level does not have type unit"+a.a+" "+a.b);
		}

		def deref(a:(Id.T, Type.T)) {
			a match {
			case (x, y) => extenv = extenv + (x -> deref_typ(y))
			}
		}
		println(extenv)
		extenv.map(deref)
		println(extenv)
		var rc = deref_term(e)
		println(extenv)
		rc
	} catch {
		case ee =>  throw ee;
	}
	
	def f(e:Syntax.T):Syntax.T = f1(e.asInstanceOf[T]).asInstanceOf[Syntax.T]
}
