package typing;
import syntax._;
import scala.collection.mutable.HashMap;

case class Unify(a:typ.T, b:typ.T) extends Exception
case class Error(a:T, b:typ.T, c:typ.T) extends Exception
case class Invalid() extends Exception

object Typing extends Syntax {

	// type inference/reconstruction
	var extenv = new HashMap[Any,Option[typ.T]]

	// for pretty printing (and type normalization) 
	// 型変数を中身でおきかえる関数
	def deref_typ(x:typ.T):typ.T = x match {
		case typ.Fun(t1s, t2) => typ.Fun(t1s.map(deref_typ), deref_typ(t2))
		case typ.Tuple(ts) => typ.Tuple(ts.map(deref_typ))
		case typ.Array(t) => typ.Array(deref_typ(t))
		case r@typ.Var(None) =>
			println( "uninstantiated type variable detected; assuming int"+r+".")
			r.a = Some(typ.Int())
			typ.Int()
		case r@typ.Var(Some(t)) =>
			val t1 = deref_typ(t)
			r.a = Some(t1)
			t1
		case t => t
	}
	
	def deref_id_typ (a:(id.T, typ.T)):(id.T, typ.T) = a match {
		case (x,t) => (x, deref_typ(t))
	}
	def deref_term(e:T):T = e match {
		case Not(e) => Not(deref_term(e))
		case Neg(e) => Neg(deref_term(e))
		case Add(e1, e2) => Add(deref_term(e1), deref_term(e2))
		case Sub(e1, e2) => Sub(deref_term(e1), deref_term(e2))
		case Eq(e1, e2) => Eq(deref_term(e1), deref_term(e2))
		case Le(e1, e2) => Le(deref_term(e1), deref_term(e2))
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
	def occur(r1:Option[typ.T],r2:typ.T):Boolean = {
		r2 match { // occur check
		case typ.Fun(t2s/*:List[typ.T]*/, t2:typ.T)
		                               => t2s.exists(occur(r1,_)) || occur(r1, t2)
		case typ.Tuple(t2s)            => t2s.exists(occur(r1,_))
		case typ.Array(t2)             => occur(r1, t2)
		case typ.Var(None)             => false
		case typ.Var(r2) if (r1 == r2) => true
		case typ.Var(Some(t2))         => occur(r1, t2)
		case _ => false
		}
	}

	def iter2(f:(typ.T,typ.T)=>scala.Unit,x:List[typ.T],y:List[typ.T]):scala.Unit = (x,y) match {
		case (a::as,b::bs) => f(a,b); iter2(f,as,bs)
		case (List(),List()) =>
		case a => throw new Invalid()
	}

	def add_list(xys:List[(id.T, typ.T)], env:HashMap[Any,Option[typ.T]]):HashMap[Any,Option[typ.T]] = {
		xys.foldLeft(env) {
			(x,y) => y match {
			case (a, b) => env + (x->Some(b))
			}
			env
		}
		env
	}

	// 型が合うように、型変数への代入をする
	def unify(t1:typ.T,t2:typ.T) {
		(t1, t2) match {
		case (typ.Unit(), typ.Unit()) | (typ.Bool(), typ.Bool()) | (typ.Int(), typ.Int()) | (typ.Float(), typ.Float()) =>
		case (typ.Fun(t1s, t11), typ.Fun(t2s, t21)) =>
			try {
				iter2 (unify, t1s, t2s)
			} catch {
				case Invalid() => throw Unify(t1, t2);
			}
			unify(t11, t21)
		case (typ.Tuple(t1s), typ.Tuple(t2s)) =>
			try {
				iter2 (unify,t1s, t2s)
			} catch {
				case Invalid() => throw Unify(t1, t2)
			}
		case (typ.Array(t1), typ.Array(t2)) => unify(t1, t2)

		case (typ.Var(r1), typ.Var(r2)) if(r1 == r2) => ()
		case (typ.Var(Some(t1dash)), _) => unify(t1dash, t2)
		case (_, typ.Var(Some(t2dash))) => unify(t1, t2dash)
		case (r@typ.Var(r1@None), _) => // 一方が未定義の型変数の場合
			if (occur(r1, t2)) {// 両方未定義ならエラー
				throw Unify(t1, t2);
			}
			r.a = Some(t2)
		case (_, r@typ.Var(r2@None)) =>
			if (occur (r2, t1)) {// 両方未定義ならエラー
				throw new Unify(t1, t2)
			}
			r.a = Some(t1)
		case (_, _) => throw new Unify(t1, t2)
		}
	}

	def snd(a:(id.T,typ.T)):typ.T = a match {case (_,b) => b }
	def g1(env:HashMap[Any,Option[typ.T]]):(T)=>typ.T = g(env,_)
	// 型推論ルーチン
	def g(env:HashMap[Any,Option[typ.T]], e:T):typ.T = try {
		def gint(e1:T,e2:T):typ.T = {
			unify(typ.Int(), g(env, e1))
			unify(typ.Int(), g(env,e2))
			typ.Int()
		}
		def gfloat(e1:T, e2:T):typ.T = {
			unify(typ.Float(), g(env,e1))
			unify(typ.Float(), g(env,e2))
			typ.Float()
		}
		def gbool(e1:T, e2:T):typ.T = {
			unify(g(env, e1), g(env, e2))
			typ.Bool()
		}
		e match {
		case Unit() => typ.Unit()
		case Bool(_) => typ.Bool()
		case Int(_) => typ.Int()
		case Float(_) => typ.Float()
		case Not(e) => unify(typ.Bool(),g(env, e)); typ.Bool()
		case Neg(e) => unify(typ.Int(),g(env, e)); typ.Int()
		case Add(e1, e2) => gint(e1,e2)
		case Sub(e1, e2) => gint(e1,e2) // 足し算（と引き算）の型推論
		case FNeg(e) => unify(typ.Float(), g(env, e)); typ.Float()
		case FAdd(e1, e2) => gfloat(e1,e2)
		case FSub(e1, e2) => gfloat(e1,e2)
		case FMul(e1, e2) => gfloat(e1,e2)
		case FDiv(e1, e2) => gfloat(e1,e2)
		case Eq(e1, e2) => gbool(e1,e2)
		case Le(e1, e2) => gbool(e1,e2)
		case If(e1, e2, e3) =>
			unify(g(env, e1), typ.Bool())
			val t2 = g(env, e2)
			val t3 = g(env, e3)
			unify(t2, t3)
			t2
		case Let((x, t), e1, e2) => // letの型推論
			unify(t, g(env,e1));
			print(e + " env="+env+ " x="+x+ " t = "+ t);
			env + (x -> Some(t));
			g(env, e2)
		case Var(x) if (env.get(x)!=None) => env.get(x).get.get // 変数の型推論
		case Var(x) if (extenv.get(x)!=None) => extenv.get(x).get.get
		case a@Var(x) => // 外部変数の型推論
			val t = typ.Type.gentyp()
			println("free variable "+ x + " assumed as external "+a+"."+t)
			extenv + (x-> Some(t))
			t
		case LetRec(Fundef((x, t),yts,e1), e2) => // let recの型推論
			env + (x -> Some(t))
			unify(t, typ.Fun(
				yts.map(snd), g(add_list(yts, env), e1)
			))
			g(env, e2)
		case App(e, es) => // 関数適用の型推論
			val t = typ.Type.gentyp()
			val a = es.map(g1(env));
			val b = g(env, e);
			unify (b, typ.Fun(a, t))
			t
		case Tuple(es) => typ.Tuple(es.map(g1(env)))
		case LetTuple(xts, e1, e2) =>
			unify (typ.Tuple(xts.map(snd)), g(env, e1))
			g(add_list(xts, env), e2)
		case Array(e1, e2) => // must be a primitive for "polymorphic" typing
			unify(g(env, e1), typ.Int())
			typ.Array(g(env, e2))
		case Get(e1, e2) =>
			val t = typ.Type.gentyp()
			unify(typ.Array(t), g(env, e1))
			unify(typ.Int(), g(env, e2))
			t
		case Put(e1, e2, e3) =>
			val t = g(env, e3)
			unify(typ.Array(t), g(env,e1))
			unify(typ.Int(), g(env, e2))
			typ.Unit()
		}
	} catch {
		case a@Unify(t1, t2) => a.printStackTrace(); println(t1,t2); throw new Error(deref_term(e), deref_typ(t1), deref_typ(t2))
	}

	def f(e:T):T = try {
		extenv = new HashMap[Any,Option[typ.T]]
/*		deref_typ(g(new HashMap[Any,Option[typ.T]], e)) match {
		case typ.Unit() => Unit
		case a => println("warning: final result does not have type unit"+a+"."); Unit
		}*/
		try {
			unify(typ.Unit(), g(new HashMap[Any,Option[typ.T]],e))
		} catch {
		case a@Unify(_,_) => a.printStackTrace(); throw new Exception("top level does not have type unit"+a.a+" "+a.b);
		}

		def deref(a:(Any, Option[typ.T])) {
			a match {
			case (x,Some(y)) => extenv.update(x,Some(deref_typ(y)))
			case x =>
			}
		}
		extenv.map(deref)
		deref_term(e)
	} catch {
		case ee =>  throw ee;
	}
}
