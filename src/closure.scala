package mincaml;

object Closure {
	case class Closure(entry:Id.L, actual_fv:List[Id.T])

	abstract sealed class T() // クロージャ変換後の式 (caml2html: closure_t)
	case class Unit() extends T
	case class Int(a:scala.Int) extends T
	case class Float(a:scala.Double) extends T
	case class Neg(a:Id.T) extends T
	case class Add(a:Id.T, b:Id.T) extends T
	case class Sub(a:Id.T, b:Id.T) extends T
	case class FNeg(a:Id.T) extends T
	case class FAdd(a:Id.T, b:Id.T) extends T
	case class FSub(a:Id.T, b:Id.T) extends T
	case class FMul(a:Id.T, b:Id.T) extends T
	case class FDiv(a:Id.T, b:Id.T) extends T
	case class IfEq(a:Id.T, b:Id.T, c:T, d:T) extends T
	case class IfLE(a:Id.T, b:Id.T, c:T, d:T) extends T
	case class Let(a:(Id.T, Type.T), b:T, c:T) extends T
	case class Var(a:Id.T) extends T
	case class MakeCls(a:(Id.T, Type.T), b:Closure, c:T) extends T
	case class AppCls(a:Id.T, b:List[Id.T]) extends T
	case class AppDir(a:Id.L, b:List[Id.T]) extends T
	case class Tuple(a:List[Id.T]) extends T
	case class LetTuple(a:List[(Id.T, Type.T)], b:Id.T, c:T) extends T
	case class Get(a:Id.T, b:Id.T) extends T
	case class Put(a:Id.T, b:Id.T, c:Id.T) extends T
	case class ExtArray(a:Id.L) extends T

	case class Fundef(name:(Id.L, Type.T),args:List[(Id.T,Type.T)], formal_fv:List[(Id.T, Type.T)], body:T)
	case class Prog(a:List[Fundef],b:T)

	def fv(e:T):Set[Id.T] = e match {
		case Unit() | Int(_) | Float(_) | ExtArray(_) => Set()
		case Neg(x) => Set(x)
		case FNeg(x) => Set(x)
		case Add(x, y)  => Set(x, y)
		case Sub(x, y)  => Set(x, y)
		case FAdd(x, y) => Set(x, y)
		case FSub(x, y) => Set(x, y)
		case FMul(x, y) => Set(x, y)
		case FDiv(x, y) => Set(x, y)
		case Get(x, y)  => Set(x, y)
		case IfEq(x, y, e1, e2) => (fv(e1) ++ fv(e2)) + y + x
		case IfLE(x, y, e1, e2) => (fv(e1) ++ fv(e2)) + y + x
		case Let((x, t), e1, e2) => fv(e1) ++ (fv(e2) - x)
		case Var(x) => Set(x)
		case MakeCls((x, t), Closure(l,ys), e) => ((Set() ++ ys) ++ fv(e)) - x 
		case AppCls(x, ys) => Set() ++ (x :: ys)
		case AppDir(_, xs) => Set() ++ (xs)
		case Tuple(xs)     => Set() ++ (xs)
		case LetTuple(xts, y, e) => (fv(e) -- (Set() ++ xts.map{case(a,_)=>a} )) - y
		case Put(x, y, z) => Set(x, y, z)
	}

	var toplevel:List[Fundef] = List()

	// クロージャ変換ルーチン本体 (caml2html: closure_g)
	def g(env:Map[Id.T,Type.T], known:Set[Id.T], e:KNormal.T):T = e match {
		case KNormal.Unit() => Unit()
		case KNormal.Int(i) => Int(i)
		case KNormal.Float(d) => Float(d)
		case KNormal.Neg(x) => Neg(x)
		case KNormal.Add(x, y) => Add(x, y)
		case KNormal.Sub(x, y) => Sub(x, y)
		case KNormal.FNeg(x) => FNeg(x)
		case KNormal.FAdd(x, y) => FAdd(x, y)
		case KNormal.FSub(x, y) => FSub(x, y)
		case KNormal.FMul(x, y) => FMul(x, y)
		case KNormal.FDiv(x, y) => FDiv(x, y)
		case KNormal.IfEq(x, y, e1, e2) => IfEq(x, y, g(env,known,e1), g(env,known,e2))
		case KNormal.IfLE(x, y, e1, e2) => IfLE(x, y, g(env,known,e1), g(env,known,e2))
		case KNormal.Let((x, t), e1, e2) => Let((x, t), g(env,known,e1), g(env + (x -> t), known, e2))
		case KNormal.Var(x) => Var(x)
		case KNormal.LetRec(KNormal.Fundef((x, t),yts,e1), e2) => // 関数定義の場合 (caml2html: closure_letrec)
			// 関数定義let rec x y1 ... yn = e1 in e2の場合は、
			// xに自由変数がない(closureを介さずdirectに呼び出せる)
			// と仮定し、knownに追加してe1をクロージャ変換してみる *)
			val toplevel_backup = toplevel;
			val envdash = env + (x -> t);
			val knowndash = known + x;
			val e1dash = g(envdash ++ yts, knowndash, e1);
			// 本当に自由変数がなかったか、変換結果e1dashを確認する
			// 注意: e1dashにx自身が変数として出現する場合はclosureが必要!
			// (thanks to nuevo-namasute and azounoman; test/cls-bug2.ml参照)
			val zs = fv(e1dash) ** (Set() ++ (yts.map{case(a,_)=>a}))
			val (knowndash2, e1dash2) =
				if (zs.isEmpty) {
					(knowndash, e1dash)
				} else {
					// 駄目だったら状態(toplevelの値)を戻して、クロージャ変換をやり直す
					println("free variable(s) "+Id.pp_list(zs.toList)+" found in function "+x+"@.");
					println("function "+x+" cannot be directly applied in fact@.");
					toplevel = toplevel_backup;
					val e1dasha = g(envdash ++ yts, known, e1);
					(known, e1dasha)
				}
			// 自由変数のリスト
			val zs2 = (fv(e1dash2) -- (Set(x)++ yts.map{case(a,_)=>a}) ).toList ;
			
			// ここで自由変数zの型を引くために引数envが必要
			val zts = zs2.map(z => (z, envdash(z)) );
			toplevel = Fundef((Id.L(x.s), t), yts, zts, e1dash2) :: toplevel; // トップレベル関数を追加
			val e2dash = g(envdash,knowndash2,e2);

			// xが変数としてe2dashに出現するか
			if (fv(e2dash).contains(x)) {
				MakeCls((x, t), Closure(Id.L(x.s), zs2), e2dash) // 出現していたら削除しない
			} else {
				println("eliminating closure(s) "+x+"@.");
				e2dash
			} // 出現しなければMakeClsを削除
		case KNormal.App(x, ys) if (known(x)) => // 関数適用の場合 (caml2html: closure_app)
			println("directly applying "+x+"@.");
			AppDir(Id.L(x.s), ys)
		case KNormal.App(f, xs) => AppCls(f, xs)
		case KNormal.Tuple(xs) => Tuple(xs)
		case KNormal.LetTuple(xts, y, e) => LetTuple(xts, y, g(env ++ xts, known, e))
		case KNormal.Get(x, y) => Get(x, y)
		case KNormal.Put(x, y, z) => Put(x, y, z)
		case KNormal.ExtArray(x) => ExtArray(Id.L(x.s))
		case KNormal.ExtFunApp(x, ys) => AppDir(Id.L("min_caml_" + x), ys)
	}

	def f(e:KNormal.T):Prog = {
		toplevel = List()
		val edash = g(Map(), Set(), e)
		Prog(toplevel.reverse, edash)
	}
}
