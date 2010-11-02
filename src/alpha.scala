package mincaml;
import scala.collection.immutable.HashMap;

/**
 * α変換
 *
 * 異なる変数には異なる名前をつける
 * rename identifiers to make them unique (alpha-conversion)
 */
object Alpha extends KNormal {

	/**
	 * 外部変数は変換しない
	 *
	 * 外部変数の名前はenvに登録されないので、α変換の対象になりません。
	 * これは意図された動作です。
	 * もし外部変数の名前が変わってしまったら正しくリンクできなくなるからです。
	 */
	def find(x:Id.T, env:Map[Id.T, Id.T]):Id.T = {
		try {
			env(x)
		} catch {
			case _ => x
		}
	}

	/**
	 * α変換ルーチン本体
	 *
	 * 変換前の変数名から変換後の変数名への写像envと、変換前の式eとを受け取り、変換後の式を返します。
	 * たとえばlet x = e1 in e2という式があったら、まずe1を変換してから、
	 * 新しい変数x'を作り、xからx'への対応をenvに追加して、e2を変換するという具合です。
	 * let recやLetTupleの場合も、一見するとややこしそうですが、やっていることは一緒です。
	 */
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

	/**
	 * α変換エントリポイント
	 *
	 * さて、K正規化が済んだら最適化を行うのですが、その前に「異なる変数には異なる名前をつける」α変換を行います。
	 * 違う変数に同じ名前がついていると、いろいろと処理が面倒になるためです。
	 * たとえばlet x = 123 in let x = 456 in x + xという式だったら、
	 * let x1 = 123 in let x2 = 456 in x2 + x2と直してしまいます。
	 *
	 * 環境を作ってgを呼び出します。
	 * @param e: KNormal.T K正規化の式
	 * @return KNormal.T α変換後のK正規化された式
	 */
	def apply(e: KNormal.T): KNormal.T = {
		g(
			Map[Id.T, Id.T](),
			e.asInstanceOf[T]
		).asInstanceOf[KNormal.T]
	}
}
