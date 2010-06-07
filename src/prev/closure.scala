case class Closure(entry:Id.l, actual_fv:List[Id.t])

abstract sealed class T() // クロージャ変換後の式 (caml2html: closure_t)
case class Unit() extends T
case class Int(a:int) extends T
case class Float(a:float) extends T
case class Neg(a:Id.t) extends T
case class Add(a:Id.t, b:Id.t) extends T
case class Sub(a:Id.t, b:Id.t) extends T
case class FNeg(a:Id.t) extends T
case class FAdd(a:Id.t, b:Id.t) extends T
case class FSub(a:Id.t, b:Id.t) extends T
case class FMul(a:Id.t, b:Id.t) extends T
case class FDiv(a:Id.t, b:Id.t) extends T
case class IfEq(a:Id.t, b:Id.t, c:t, d:t) extends T
case class IfLE(a:Id.t, b:Id.t, c:t, d:t) extends T
case class Let(a:(Id.t, Type.t), b:t, c:t) extends T
case class Var(a:Id.t) extends T
case class MakeCls(a:(Id.t, Type.t), b:Closure, c:t) extends T
case class AppCls(a:Id.t, List[b:Id.t]) extends T
case class AppDir(a:Id.l, List[b:Id.t]) extends T
case class Tuple(a:Id.t list) extends T
case class LetTuple(a:List[(Id.t, Type.t)], b:Id.t, c:t) extends T
case class Get(a:Id.t, b:Id.t) extends T
case class Put(a:Id.t, b:Id.t, c:Id.t) extends T
case class ExtArray(a:Id.l) extends T

case class Fundef(name:(Id.l, Type.t),args:List[(Id.t,Type.t)], formal_fv:List[(Id.t, Type.t)], body:T)
case class Prog(a:List[Fundef],b:T)

def fv(e) = e match {
	case Unit | Int(_) | Float(_) | ExtArray(_) => S.empty()
	case Neg(x) => S.singleton(x)
	case FNeg(x) => S.singleton(x)
	case Add(x, y)  => S.of_list(List(x, y))
	case Sub(x, y)  => S.of_list(List(x, y))
	case FAdd(x, y) => S.of_list(List(x, y))
	case FSub(x, y) => S.of_list(List(x, y))
	case FMul(x, y) => S.of_list(List(x, y))
	case FDiv(x, y) => S.of_list(List(x, y))
	case Get(x, y)  => S.of_list(List(x, y))
	case IfEq(x, y, e1, e2) => S.add(x, S.add(y, S.union(fv(e1), fv(e2)) )
	case IfLE(x, y, e1, e2) => S.add(x, S.add(y, S.union(fv(e1), fv(e2)) )
	case Let((x, t), e1, e2) => S.union( fv(e1),S.remove(x,fv(e2)) )
	case Var(x) => S.singleton(x)
	case MakeCls((x, t), Closure(l,ys), e) => S.remove(x, S.union(S.of_list(ys),fv(e)) )
	case AppCls(x, ys) => S.of_list(x :: ys)
	case AppDir(_, xs) => S.of_list(xs)
	case Tuple(xs)     => S.of_list(xs)
	case LetTuple(xts, y, e) => S.add(y, S.diff(fv(e)), S.of_list(xts.map(fst)) )
	case Put(x, y, z) => S.of_list(List(x, y, z))
}

var toplevel:List[Fundef] = List()

// クロージャ変換ルーチン本体 (caml2html: closure_g)
def g(env, known, e) = e match {
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
	case KNormal.Let((x, t), e1, e2) => Let((x, t), g(env,known,e1), g(M.add(x, t, env), known, e2))
	case KNormal.Var(x) => Var(x)
	case KNormal.LetRec(KNormal.Fundef((x, t),yts,e1), e2) => // 関数定義の場合 (caml2html: closure_letrec)
		// 関数定義let rec x y1 ... yn = e1 in e2の場合は、
		// xに自由変数がない(closureを介さずdirectに呼び出せる)
		// と仮定し、knownに追加してe1をクロージャ変換してみる *)
		val toplevel_backup = toplevel;
		val envdash = M.add(x, t, env);
		val knowndash = S.add(x, known);
		val e1dash = g(M.add_list(yts, envdash), knowndash, e1);
		// 本当に自由変数がなかったか、変換結果e1dashを確認する
		// 注意: e1dashにx自身が変数として出現する場合はclosureが必要!
		// (thanks to nuevo-namasute and azounoman; test/cls-bug2.ml参照)
		val zs = S.diff(fv(e1dash), S.of_list(yts.map(fst)))
		val (knowndash, e1dash) =
			if (S.is_empty(zs)) {
				(knowndash, e1dash)
			} else {
				// 駄目だったら状態(toplevelの値)を戻して、クロージャ変換をやり直す
				println("free variable(s) "+Id.pp_list(S.elements(zs))+" found in function "+x+"@.");
				println("function "+x+" cannot be directly applied in fact@.");
				toplevel = toplevel_backup;
				val e1dash = g(M.add_list(yts, envdash), known, e1);
				(known, e1dash)
			}
		// 自由変数のリスト
		val zs = S.elements(S.diff(fv(e1dash)), S.add(x, S.of_list(yts.map(fst)) ) ;
		
		// ここで自由変数zの型を引くために引数envが必要
		val zts = zs.map(z => (z, M.find(z,envdash)) );
		toplevel = Fundef((Id.L(x), t), yts, zts, e1dash) :: toplevel; // トップレベル関数を追加
		val e2dash = g(envdash,knowndash,e2);

		// xが変数としてe2dashに出現するか
		if (S.mem(x, fv(e2dash))) {
			MakeCls((x, t), Closure(Id.L(x), zs), e2dash) // 出現していたら削除しない
		} else {
			println("eliminating closure(s) "+x+"@.");
			e2dash
		} // 出現しなければMakeClsを削除
	case KNormal.App(x, ys) when S.mem x known => // 関数適用の場合 (caml2html: closure_app)
		println("directly applying "+x+"@.");
		AppDir(Id.L(x), ys)
	case KNormal.App(f, xs) => AppCls(f, xs)
	case KNormal.Tuple(xs) => Tuple(xs)
	case KNormal.LetTuple(xts, y, e) => LetTuple(xts, y, g(M.add_list(xts, env), known, e))
	case KNormal.Get(x, y) => Get(x, y)
	case KNormal.Put(x, y, z) => Put(x, y, z)
	case KNormal.ExtArray(x) => ExtArray(Id.L(x))
	case KNormal.ExtFunApp(x, ys) => AppDir(Id.L("min_caml_" + x), ys)
}
def f(e) = {
  toplevel = List()
  val edash = g(M.empty(), S.empty(), e)
  Prog(List.rev(toplevel), edash)
}
