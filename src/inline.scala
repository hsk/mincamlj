package mincaml;
import scala.collection.immutable.HashMap;

object Inline extends KNormal {

	// インライン展開する関数の最大サイズ (caml2html: inline_threshold)
	var threshold = 0 // Mainで-inlineオプションによりセットされる

	def size(e:T):scala.Int = e match {
		case IfEq(_, _, e1, e2)         => 1 + Inline.size(e1) + Inline.size(e2)
		case IfLE(_, _, e1, e2)         => 1 + Inline.size(e1) + Inline.size(e2)
		case Let(_, e1, e2)             => 1 + Inline.size(e1) + Inline.size(e2)
		case LetRec(Fundef(_,_,e1), e2) => 1 + Inline.size(e1) + Inline.size(e2)
		case LetTuple(_, _, e)          => 1 + Inline.size(e)
		case _                          => 1
	}


/*
val g : 
  ((M.key * Type.t) list * KNormal.t) M.t 
  
  -> KNormal.t -> KNormal.t = <fun>
*/

	// インライン展開ルーチン本体 (caml2html: inline_g)
	def g(env:Map[Id.T, (List[(Id.T, Type.T)],T)], e:T):T = e match {
		case IfEq(x, y, e1, e2) => IfEq(x, y, g(env, e1), g(env, e2))
		case IfLE(x, y, e1, e2) => IfLE(x, y, g(env, e1), g(env, e2))
		case Let(xt, e1, e2)    => Let(xt, g(env, e1), g(env, e2))

		// 関数定義の場合 (caml2html: inline_letrec)
		case LetRec(Fundef((x, t), yts, e1), e2) => 
			val env2 = if (size(e1) > threshold) {
					env
				} else {
					env + (x->(yts, e1))
				}
			LetRec(Fundef((x, t), yts, g(env, e1)), g(env2, e2))
		// 関数適用の場合 (caml2html: inline_app)
		case App(x, ys) if (env.get(x) != None) => 
			val (zs, e) = env(x);
			println( "inlining "+x+"@.");
			val envdash = foldLeft2(
					new HashMap[Id.T, Id.T],
					zs,
					ys,
					(envdash, x, y) => {x match { case (z,t)=>envdash + (z -> y)}}
				);
			Alpha.g(envdash, e.asInstanceOf[Alpha.T]).asInstanceOf[Inline.T]
		case LetTuple(xts, y, e) => LetTuple(xts, y, g(env, e))
		case e => e
	}

	def foldLeft2(
		env:Map[Id.T,Id.T],
		zs:List[(mincaml.Id.T, mincaml.Type.T)],
		ys:List[Id.T],
		f:(Map[Id.T,Id.T],(Id.T,Type.T),Id.T)=>Map[Id.T, Id.T]
	):Map[Id.T,Id.T] = (zs,ys) match {
		case (z::List(), y::List()) => f(env, z, y)
		case (z::zs, y::ys) =>
			val env2:Map[Id.T, Id.T] = f(env, z, y);
			foldLeft2(env2, zs, ys, f)
		case _ => throw new Exception();
	}
	
	def f(e:KNormal.T):KNormal.T = g(new HashMap[Id.T, (List[(Id.T, Type.T)],T)], e.asInstanceOf[T]).asInstanceOf[KNormal.T]
}
