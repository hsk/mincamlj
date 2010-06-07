// translation into SPARC assembly with infinite number of virtual registers

open X86Asm

var data = List() // 浮動小数の定数テーブル (caml2html: virtual_data)

def classify(xts, ini, addf, addi) = {
	List.fold_left(
		(acc, (x, t)) => t match {
			case typ.Unit() => acc
			case typ.Float() => addf(acc, x)
			case _ => addi(acc, x, t)
		},
		ini,
		xts
	)
}
def separate(xts) = {
	classify(
		xts,
		(List(), List()),
		((int, float), x)    => (int, float ::: List(x))),
		((int, float), x, _) => (int ::: List(x), float))
	)
}

def expand(xts, ini, addf, addi) = {
	classify(
		xts,
		ini,
		((offset, acc), x) => {
			val offset = align(offset);
			(offset + 8, addf(x, offset, acc))
		},
		((offset, acc), x, t) => (offset + 4, addi(x, t, offset, acc))
	)
}

// 式の仮想マシンコード生成 (caml2html: virtual_g)
def g(env, e) = e match {
	case Closure.Unit() => Ans(Nop)
	case Closure.Int(i) => Ans(Set(i))
	case Closure.Float(d) =>
		val l =
			try {
				// すでに定数テーブルにあったら再利用
				val (l, _) = List.find(
					(_, ddash) => { d == ddash },
					data
				);
				l
			} catch {
				case Not_found =>
					val l = Id.L(Id.genid "l");
					data = (l, d) :: data;
					l
			}
		val x = Id.genid("l");
		Let((x, Type.Int), SetL(l), Ans(LdDF(x, C(0))))

	case Closure.Neg(x) => Ans(Neg(x))
	case Closure.Add(x, y) => Ans(Add(x, V(y)))
	case Closure.Sub(x, y) => Ans(Sub(x, V(y)))
	case Closure.FNeg(x) => Ans(FNegD(x))
	case Closure.FAdd(x, y) => Ans(FAddD(x, y))
	case Closure.FSub(x, y) => Ans(FSubD(x, y))
	case Closure.FMul(x, y) => Ans(FMulD(x, y))
	case Closure.FDiv(x, y) => Ans(FDivD(x, y))
	case Closure.IfEq(x, y, e1, e2) =>
		M.find(x, env) match {
			case Type.Bool | Type.Int => Ans(IfEq(x, V(y), g(env, e1), g(env, e2)))
			case Type.Float => Ans(IfFEq(x, y, g(env, e1), g(env, e2)))
			case _ => failwith "equality supported only for bool, int, and float")
		}
	case Closure.IfLE(x, y, e1, e2) =>
		M.find(x, env) match {
			case Type.Bool | Type.Int => Ans(IfLE(x, V(y), g(env, e1), g(env, e2)))
			case Type.Float => Ans(IfFLE(x, y, g(env, e1), g(env, e2)))
			case _ => failwith( "inequality supported only for bool, int and float")
		}
	case Closure.Let((x, t1), e1, e2) =>
		val e1dash = g(env, e1);
		val e2dash = g(M.add(x, t1, env), e2);
		concat(e1dash, (x, t1), e2dash)
	case Closure.Var(x) =>
		M.find(x, env) match {
			case Type.Unit => Ans(Nop)
			case Type.Float => Ans(FMovD(x))
			case _ => Ans(Mov(x))
		}
	case Closure.MakeCls((x, t), Closure.Closure(l, ys), e2) => // クロージャの生成 (caml2html: virtual_makecls)
		// Closureのアドレスをセットしてから、自由変数の値をストア
		val e2dash = g(M.add(x, t, env), e2);
		val (offset, store_fv) =
		expand (
			List.map(y => (y, M.find(y, env)), ys),
			(4, e2dash),
			(y, offset, store_fv) => seq(StDF(y, x, C(offset)), store_fv),
			(y, _, offset, store_fv) => seq(St(y, x, C(offset)), store_fv)
		);
		Let((x, t), Mov(reg_hp),
			Let(
				(reg_hp, Type.Int),
				Add(reg_hp, C(align(offset))),
				{
					val z = Id.genid("l");
					Let(
						(z, Type.Int), SetL(l),
						seq(
							St(z, x, C(0)),
							store_fv
						)
					)
				}
			)
		)
	case Closure.AppCls(x, ys) =>
		val (int, float) = separate(
			ys.map(
				y => (y, M.find(y, env))
			)
		);
		Ans(CallCls(x, int, float))
	case Closure.AppDir(Id.L(x), ys) =>
		val (int, float) = separate(
			ys.map(
				y => (y, M.find(y, env))
			)
		);
		Ans(CallDir(Id.L(x), int, float))
	case Closure.Tuple(xs) => // 組の生成 (caml2html: virtual_tuple)
		val y = Id.genid("t");
		val (offset, store) =
			expand(
				xs.map( x => (x, M.find(x, env)) ),
				(0, Ans(Mov(y))),
				(x, offset, store)    => seq(StDF(x, y, C(offset)), store),
				(x, _, offset, store) => seq(St  (x, y, C(offset)), store)
			);
		Let((	y,
				Type.Tuple(
					xs.map (x => M.find(x, env))
				)
			),
			Mov(reg_hp),
			Let((reg_hp, Type.Int),
				Add(reg_hp, C(align(offset))),
				store
			)
		)
	case Closure.LetTuple(xts, y, e2) =>
		val s = Closure.fv(e2);
		val (offset, load) =
			expand(
				xts,
				(0, g(M.add_list(xts, env), e2)),
				(x, offset, load) => {
					if (not(S.mem(x, s))) {
						load
					} else { // [XX] a little ad hoc optimization
						fletd(x, LdDF(y, C(offset)), load)
					}
				},
				(x, t, offset, load) => {
					if (not(S.mem(x, s))) {
						load
					} else {// [XX] a little ad hoc optimization
						Let((x, t), Ld(y, C(offset)), load)
					}
				}
			);
		load
	case Closure.Get(x, y) => // 配列の読み出し (caml2html: virtual_get)
		val offset = Id.genid("o");
		M.find(x, env) match {
			case Type.Array(Type.Unit) => Ans(Nop)
			case Type.Array(Type.Float) =>
				Let((offset, Type.Int), SLL(y, C(3)),
					Ans(LdDF(x, V(offset))))
			case Type.Array(_) =>
				Let((offset, Type.Int), SLL(y, C(2)),
					Ans(Ld(x, V(offset))))
			case _ => assert(false)
		}
	case Closure.Put(x, y, z) =>
		val offset = Id.genid("o");
		M.find(x, env) match {
			case Type.Array(Type.Unit()) => Ans(Nop)
			case Type.Array(Type.Float()) =>
				Let((offset, Type.Int()), SLL(y, C(3)),
					Ans(StDF(z, x, V(offset))))
			case Type.Array(_) =>
				Let((offset, Type.Int()), SLL(y, C(2)),
					Ans(St(z, x, V(offset))))
			case _ => assert(false)
		}
	case Closure.ExtArray(Id.L(x)) => Ans(SetL(Id.L("min_caml_" + x)))
}


// 関数の仮想マシンコード生成 (caml2html: virtual_h)
def h(e) = e match {
case Closure.Closure((Id.L(x), t); yts; zts; e) =>
	val (int, float) = separate(yts);
	val (offset, load) =
		expand(
		  zts,
		  (
		    4,
		    g(
		      M.add(
		        x,
		        t,
		        M.add_list(yts, M.add_list(zts, M.empty()))
		      ),
		      e
		    )
		  ),
		  z(offset, load) => fletd(z, LdDF(reg_cl, C(offset)), load),
		  z(t, offset, load) => Let((z, t), Ld(reg_cl, C(offset)), load)
		);
	t match {
		case Type.Fun(_, t2) =>
			{ name = Id.L(x); args = int; fargs = float; body = load; ret = t2 }
		case _ => assert(false)
	}
}

// プログラム全体の仮想マシンコード生成 (caml2html: virtual_f)
let f (Closure.Prog(fundefs, e)) = {
	data = List();
	val fundefs = fundefs.map(h);
	val e = g(M.empty(), e)
	Prog(data, fundefs, e)
}
