package mincaml;
import scala.collection.immutable._;

// give names to intermediate values (K-normalization)

class KNormal {

	/**
	 * K正規化後の式
	 */
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

	/**
	 * 式に出現する（自由な）変数
	 */
	def fv(e:T):Set[Id.T] = e match {
		case Unit() | Int(_) | Float(_) | ExtArray(_) => Set()
		case Neg(x) => Set(x)
		case FNeg(x) => Set(x)
		case Add(x, y) => Set(x, y)
		case Sub(x, y) => Set(x, y)
		case FAdd(x, y) => Set(x, y)
		case FSub(x, y) => Set(x, y)
		case FMul(x, y) => Set(x, y)
		case FDiv(x, y) => Set(x, y)
		case IfEq(x, y, e1, e2) => fv(e1) ++ fv(e2) + y + x
		case IfLE(x, y, e1, e2) => fv(e1) ++ fv(e2) + y + x
		case Let((x, t), e1, e2) => fv(e1) ++ (fv(e2) - x)
		case Var(x) => Set(x)
		case LetRec(Fundef((x, t), yts, e1), e2) =>
			val zs = fv(e1) ** (Set() ++ yts.map{case(a,_)=>a});
			(zs ++ fv(e2)) ** Set(x)
		case App(x, ys) => Set() ++ (x :: ys)
		case Tuple(xs) => Set() ++ xs
		case ExtFunApp(_, xs) => Set()++xs
		case Put(x, y, z) => Set(x,y,z)
		case Get(x, y) => Set(x, y)
		case LetTuple(xs, y, e) => Set(y) ++ (fv(e) ** (Set()++xs.map{case(a,_)=>a}))
	}
}

/**
 * K正規化
 *
 * 計算の途中結果もすべて変数として定義する
 */
object KNormal extends KNormal {

	/**
	 * letを挿入する補助関数
	 *
	 * insert_letは、式eを受け取り、新しい変数xを作って、let x = e in ...という式を返します
	 *（ただしeが最初から変数のときは、それをxとして利用し、letは挿入しません）。
	 * inの中を作るための関数kも引数として受け取り、kをxに適用した結果を...の部分として利用します。
	 * @param e1 : (T, Type.T) 置き換え元となる式と型
	 * @param k  : (Id.T) => (T, Type.T) 関数
	 * @return (T, Type.T)
	 */
	def insert_let(e1:(T, Type.T), k:(Id.T) => (T, Type.T)) : (T, Type.T) = {
		e1 match {
			case (Var(x), _) => k(x)
			case (e, t:Type.T) =>
				val x:Id.T = Id.gentmp(t)
				val (edash, tdash) = k(x)
				(Let((x, t), e, edash), tdash)
		}
	}

	/**
	 * K正規化ルーチン本体
	 *
	 * 変数の型環境envとK正規化前の式とを受け取り、K正規化後の式と、その型とを組にして返します。
	 *（型環境を受け取ったり型を返したりするのは、letで変数を定義するときに型も付加する必要があるためで、
	 * あまりK正規化の本質とは関係がありません）。
	 * 例)
	 * e1 + e2 => g(env, e1) + g(env, e2) (e1をK正規化) => let x = g(env, e1) in let y = g(env, e1) in x + y
	 * 
	 * このようなletを挿入するために、insert_letという補助関数を使用しています。
	 *
	 * なお、KNormal.gでは、K正規化のついでに、論理値true, falseを整数1, 0に変換する処理もやっています。
	 * また、比較や条件分岐も、e1 <= e2やif e then e1 else e2ではなく、
	 * if e1 <= e2 then e3 else e4のように比較と分岐が一体となった特殊な形に直します。
	 * 通常のアセンブリでは比較と分岐が一体となっているので、そのギャップを早めに埋めてしまうためです。
	 * そのために、もし条件eが比較になっていなかったら、if e <> 0 then e1 else e2のように、条件の部分を比較に変換します。
	 * また、if (not e) then e1 else e2をif e then e2 else e1のように変換していくことにより、最後は
	 * if e1 = e2 then e3 else e4
	 * あるいは
	 * if e1 <= e2 then e3 else e4
	 * という二つの形に帰着できます。
	 * これらはK正規化とは無関係ですが、もし別々のモジュールにしてしまうと途中のデータ型を定義せねばならず面倒なので、
	 * あくまで「ついで」としてKNormal.gで一緒に実装されています。
	 *  
	 * また、外部変数の使用は、外部関数の呼び出し（KNormal.ExtFunApp）か、
	 * 外部配列の参照（KNormal.ExtArray）のどちらかに限ることとします。
	 * 大抵のアプリケーションでは、この2つで必要十分だからです。
	 * これもK正規化のついでに実現されています。
	 *
	 * @param env:Map[Id.T, Type.T] 環境(変数名と型の連想配列)
	 * @param e:Syntax.T 構文解析された式
	 * @return (T, Type.T) k正規化された式とタイプ
	 */
	def g(env:Map[Id.T, Type.T], e:Syntax.T):(T, Type.T) = {
		e match {
			case Syntax.Unit() => (Unit(), Type.Unit())
			case Syntax.Bool(b) => (Int(if (b) 1 else 0), Type.Int()) // 論理値true, falseを整数1, 0に変換
			case Syntax.Int(i) => (Int(i), Type.Int())
			case Syntax.Float(d) => (Float(d), Type.Float())
			case Syntax.Not(e) => g(env, Syntax.If(e, Syntax.Bool(false), Syntax.Bool(true)))
			case Syntax.Neg(e) => insert_let(g(env, e), x => (Neg(x), Type.Int()))

			// 足し算のK正規化
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
			case cmp@(Syntax.Eq(_, _) | Syntax.LE(_, _))=>
				g(env, Syntax.If(cmp, Syntax.Bool(true), Syntax.Bool(false)))
			case Syntax.If(Syntax.Not(e1), e2, e3) => g(env, Syntax.If(e1, e3, e2)) // notによる分岐を変換
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
				g(env, Syntax.If(Syntax.Eq(e1, Syntax.Bool(false)), e3, e2)) // 比較のない分岐を変換
			case Syntax.Let((x, t), e1, e2) =>
				val (e1dash, t1) = g(env, e1);
				val (e2dash, t2) = g(env + (x -> t), e2);
				(Let((x, t), e1dash, e2dash), t2)
			case Syntax.Var(x) if(env.contains(x)) => (Var(x), env(x))
			case Syntax.Var(x) => // 外部配列の参照 (caml2html: knormal_extarray)
				Typing.extenv(x) match {
					case t@Type.Array(_) => (ExtArray(x), t)
					case _ => throw new Exception("external variable "+ x +" does not have an array type")
				}
			case Syntax.LetRec(Syntax.Fundef((x, t), yts, e1), e2) =>
				val envdash = env + (x -> t)
				val (e2dash, t2) = g(envdash, e2)
				val (e1dash, t1) = g(envdash ++ yts, e1)
				(LetRec(Fundef((x, t), yts, e1dash), e2dash), t2)
			case Syntax.App(Syntax.Var(f), e2s) if (!env.contains(f)) => // 外部関数の呼び出し (caml2html: knormal_extfunapp)
				Typing.extenv(f) match {
					case Type.Fun(_, t) =>
						def bind (xs:List[Id.T], e:List[Syntax.T]):(T, Type.T) = e match {// "xs" are identifiers for the arguments 
							case List() => (ExtFunApp(f, xs), t)
							case e2 :: e2s => insert_let(g(env, e2), x => bind(xs ::: List(x), e2s))
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
								def bind(xs:List[Id.T], es:List[Syntax.T]):(T, Type.T) = es match {// "xs" are identifiers for the arguments 
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
						val(e2dash, t2) = g(env ++ xts, e2);
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
								case _            => "create_array"
								}
								(ExtFunApp(l, List(x, y)), Type.Array(t2))
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
	}

	/**
	 * K正規化
	 *
	 * 計算の途中結果もすべて変数として定義します。
	 *
	 * 例)
	 * a + b + c - dという式を
	 * let tmp1 = a + b in
	 * let tmp2 = tmp1 + c in
	 * tmp2 - d
	 * に変換します。
	 *（K正規化という名前は、ML Kitというコンパイラに由来）
	 *
	 * k正規化で使う環境を作ってgを呼び出します。
	 *
	 * @param e:Syntax.T 構文解析された式
	 * @return T k正規化された式
	 */
	def apply(e:Syntax.T):T = {
		val (a, b) = g(new HashMap[Id.T, Type.T], e)
		a
	}
}
