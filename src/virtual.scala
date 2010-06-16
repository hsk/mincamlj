package mincaml;

// translation into SPARC assembly with infinite number of virtual registers

object Virtual extends X86Asm {

	var data = List[(Id.L,Double)]() // 浮動小数の定数テーブル (caml2html: virtual_data)

	//val separate : ('a * Type.t) list -> 'a list * 'a list
	object separate {
		type A = Id.T;
		def apply(xts:List[(A, Type.T)]):(List[A],List[A]) = {
			classify(
				xts,
				(List[A](), List[A]()),
				{case ((int1, float1), x)    => (int1, float1 ::: List(x)) },
				{case ((int2, float2), x, _) => (int2 ::: List(x), float2) }
			)
		}

		type CA = A;
		type CB = (List[A], List[A]);
		def classify(
			xts:List[(CA,Type.T)],
			ini:CB,
			addf:(CB,CA)        => CB,
			addi:(CB,CA,Type.T) => CB
		):CB = {
			xts.foldLeft(ini) {
			case (acc, (x, t)) => t match {
					case Type.Unit() => acc
					case Type.Float() => addf(acc, x)
					case _ => addi(acc, x, t)
				}
			}
		}

	}

	/*
	('a * Type.t) list ->
	int * 'b ->
	('a -> int -> 'b -> 'b) -> ('a -> Type.t -> int -> 'b -> 'b) -> int * 'b
	*/
	object expand {
		type A = Id.T
		type B = T
		type CA = A
		type CB = (Int,B)

		def apply(
			xts:List[(A, Type.T)],
			ini:(Int, B),
			addf:(A, Int, B)=>B,
			addi:(A, Type.T, Int, B)=>B):(Int,B) = {
			classify(
				xts,
				ini,
				{case ((offset, acc), x) => val offset2 = align(offset); (offset2 + 8, addf(x, offset2, acc)) },
				{case ((offset, acc), x, t) => (offset + 4, addi(x, t, offset, acc))}
			)
		}

		def classify(
			xts:List[(CA,Type.T)],
			ini:CB,
			addf:(CB,CA)        => CB,
			addi:(CB,CA,Type.T) => CB
		):CB = {
			xts.foldLeft(ini) {
			case (acc, (x, t)) => t match {
					case Type.Unit() => acc
					case Type.Float() => addf(acc, x)
					case _ => addi(acc, x, t)
				}
			}
		}
	}

	// 式の仮想マシンコード生成 (caml2html: virtual_g)
	def g(env:Map[Id.T,Type.T], e:Closure.T):T = e match {
		case Closure.Unit() => Ans(Nop())
		case Closure.Int(i) => Ans(SET(i))
		case Closure.Float(d) =>
			val v = data.find{case (_, ddash) => d == ddash}
			val l = v match {
				case Some((l,_)) => l
				case None =>
					val l = Id.genid("l");
					data = (l, d) :: data;
					l
			}
			val x = Id.genid("l");
			Let((x, Type.Int()), SETL(l), Ans(LdDF(x, C(0))))
		case Closure.Neg(x) => Ans(Neg(x))
		case Closure.Add(x, y) => Ans(Add(x, V(y)))
		case Closure.Sub(x, y) => Ans(Sub(x, V(y)))
		case Closure.FNeg(x) => Ans(FNegD(x))
		case Closure.FAdd(x, y) => Ans(FAddD(x, y))
		case Closure.FSub(x, y) => Ans(FSubD(x, y))
		case Closure.FMul(x, y) => Ans(FMulD(x, y))
		case Closure.FDiv(x, y) => Ans(FDivD(x, y))
		case Closure.IfEq(x, y, e1, e2) =>
			env(x) match {
				case Type.Bool() | Type.Int() => Ans(IfEq(x, V(y), g(env, e1), g(env, e2)))
				case Type.Float() => Ans(IfFEq(x, y, g(env, e1), g(env, e2)))
				case _ => throw new Exception( "equality supported only for bool, int, and float")
			}
		case Closure.IfLE(x, y, e1, e2) =>
			env(x) match {
				case Type.Bool() | Type.Int() => Ans(IfLE(x, V(y), g(env, e1), g(env, e2)))
				case Type.Float() => Ans(IfFLE(x, y, g(env, e1), g(env, e2)))
				case _ => throw new Exception( "inequality supported only for bool, int and float")
			}
		case Closure.Let((x, t1), e1, e2) =>
			val e1dash = g(env, e1);
			val e2dash = g(env + (x -> t1), e2);
			concat(e1dash, (x, t1), e2dash)
		case Closure.Var(x) =>
			env(x) match {
				case Type.Unit()  => Ans(Nop())
				case Type.Float() => Ans(FMovD(x))
				case _            => Ans(Mov(x))
			}
		case Closure.MakeCls((x, t), Closure.Closure(l, ys), e2) => // クロージャの生成 (caml2html: virtual_makecls)
			// Closureのアドレスをセットしてから、自由変数の値をストア
			val e2dash = g(env + (x -> t), e2);
			val (offset, store_fv) =
			expand (
				ys.map{case y => (y, env(y))},
				(4, e2dash),
				(y, offset, store_fv) => seq(StDF(y, x, C(offset)), store_fv),
				(y, _, offset, store_fv) => seq(St(y, x, C(offset)), store_fv)
			);
			Let(
				(x, t),
				Mov(reg_hp),
				Let(
					(reg_hp, Type.Int()),
					Add(reg_hp, C(align(offset))),
					{
						val z = Id.genid("l");
						Let(
							(z, Type.Int()),
							SETL(l),
							seq(St(z, x, C(0)), store_fv)
						)
					}
				)
			)
		case Closure.AppCls(x, ys) =>
			val (int1:List[Id.T], float1:List[Id.T]) = separate( ys.map{case y => (y, env(y))} );
			Ans(CallCls(x, int1, float1))
		case Closure.AppDir(x, ys) =>
			val (int1, float1) = separate( ys.map{case y => (y, env(y))} );
			Ans(CallDir(x, int1, float1))
		case Closure.Tuple(xs) => // 組の生成 (caml2html: virtual_tuple)
			val y = Id.genid("t");
			val (offset, store) =
				expand(
					xs.map{case x => (x, env(x))},
					(0, Ans(Mov(y))),
					(x, offset, store)    => seq(StDF(x, y, C(offset)), store),
					(x, _, offset, store) => seq(St  (x, y, C(offset)), store)
				);
			Let(
				(y, Type.Tuple(xs.map{case x => env(x)})),
				Mov(reg_hp),
				Let(
					(reg_hp, Type.Int()),
					Add(reg_hp, C(align(offset))),
					store
				)
			)
		case Closure.LetTuple(xts, y, e2) =>
			val s = Closure.fv(e2);
			val (offset, load) =
				expand(
					xts,
					(0, g(env ++ xts, e2)),
					(x, offset, load1:T) => {
						if (!s.contains(x)) {
							load1
						} else { // [XX] a little ad hoc optimization
							fletd(x, LdDF(y, C(offset)), load1)
						}
					},
					(x, t, offset, load1:T) => {
						if (!s.contains(x)) {
							load1
						} else {// [XX] a little ad hoc optimization
							Let((x, t), Ld(y, C(offset)), load1)
						}
					}
				);
			load
		case Closure.Get(x, y) => // 配列の読み出し (caml2html: virtual_get)
			val offset = Id.genid("o");
			env(x) match {
				case Type.Array(Type.Unit()) => Ans(Nop())
				case Type.Array(Type.Float()) =>
					Let((offset, Type.Int()), SLL(y, C(3)),
						Ans(LdDF(x, V(offset))))
				case Type.Array(_) =>
					Let((offset, Type.Int()), SLL(y, C(2)),
						Ans(Ld(x, V(offset))))
				case _ => throw new Exception();
			}
		case Closure.Put(x, y, z) =>
			val offset = Id.genid("o");
			env(x) match {
				case Type.Array(Type.Unit()) => Ans(Nop())
				case Type.Array(Type.Float()) =>
					Let((offset, Type.Int()), SLL(y, C(3)),
						Ans(StDF(z, x, V(offset))))
				case Type.Array(_) =>
					Let((offset, Type.Int()), SLL(y, C(2)),
						Ans(St(z, x, V(offset))))
				case _ => throw new Exception();
			}
		case Closure.ExtArray(x) => Ans(SETL("min_caml_" + x))
	}


	// 関数の仮想マシンコード生成 (caml2html: virtual_h)
	/*
		case class Fundef(
			name:(Id.L, Type.T),
			args:List[(Id.T, Type.T)],
			formal_fv:List[(Id.T, Type.T)],
			body:T)
	*/
	def h(e1:Closure.Fundef):Fundef = e1 match {
		case Closure.Fundef((x, t), yts:List[(Id.T, Type.T)], zts, e) =>
			val (int1:List[Id.T], float1:List[Id.T]) = separate(yts);
			val m:Map[Id.T,Type.T] = ((Map() ++ zts) ++ yts)+(x->t);
			val (offset:Int, load1:T) =
				expand(
					zts,
					(4, g(m, e)),
					(z:Id.T, offset, load0:T)               => fletd(z, LdDF(reg_cl, C(offset)), load0),
					(z:Id.T, t:Type.T, offset:Int, load2:T) => Let((z, t), Ld(reg_cl, C(offset)), load2)
				);

			t match {
				case Type.Fun(_, t2) => Fundef(x, int1, float1, load1, t2)
				case _ => throw new Exception()
			}
	}

	//val f : Closure.prog -> X86Asm.prog
	// プログラム全体の仮想マシンコード生成 (caml2html: virtual_f)
	def f(e1:Closure.Prog):Prog = e1 match {
		case Closure.Prog(fundefs:List[Closure.Fundef], e:Closure.T) =>
			data = List();
			val fundefs2 = fundefs.map(h);
			val e2 = g(Map(), e)
			Prog(data, fundefs2, e2)
	}
}
