package mincaml;
import scala.collection.immutable._;

object RegAlloc extends X86Asm {

	// allocate a register or fail
	def alloc(cont, regenv, x, t:Type.T) = {
		assert (!regenv.contains(x));
		val all = t match {
			case Type.Unit()  => List("%g0") // dummy
			case Type.Float() => allfregs
			case _            => allregs
		}

		if (all == List("%g0")) {
			"%g0"
		} else // [XX] ad hoc optimization
		if (is_reg(x)) {
			x
		} else {
			val free = fv(cont);
			try {
				val live = // 生きているレジスタ
					free.foldLeft(S.empty){
					case (live, y) =>
						if (is_reg(y)) {
							S.add(y, live)
						} else {
							try {
								S.add(M.find(y, regenv), live)
							} catch {
							case Not_found => live
							}
						}
					};
				val r = // そうでないレジスタを探す
					all.find{case r => !live.contains(r)};
				// println("allocated " + x + " to " + r + "@.");
				r
			} catch {
			case Not_found => failwith (Printf.sprintf "register allocation failed for " + x)
			}
		}
	}

	// auxiliary function for g and gdash_and_restore
	def add(x, r, regenv) = {
		if (is_reg(x)) {
			assert (x == r);
			regenv
		} else {
			regenv + (x -> r)
		}
	}

	// auxiliary functions for gdash
	class NoReg(Id.t, Type.t) extends Exception

	def find(x, t, regenv) = {
		if (is_reg(x)) {
			x
		}else {
			try {
				regenv(x)
			} catch {
			case _ => throw new NoReg(x, t)
			}
		}
	}

	def finddash(xdash:id_or_imm, regenv) = xdash match {
		case V(x) => V(regenv((x, Type.Int())))
		case c => c
	}

	// 命令列のレジスタ割り当て (caml2html: regalloc_g)
	def g(dest, cont, regenv, e) = e match {
		case Ans(exp) => gdash_and_restore dest cont regenv exp
		case Let(xt@(x, t), exp, e) =>
			assert (not(M.mem(x, regenv)));
			val contdash = concat(e, dest, cont);
			val (e1dash, regenv1) = gdash_and_restore(xt, contdash, regenv, exp);
			val r = alloc(contdash, regenv1, x, t);
			val (e2dash, regenv2) = g(dest, cont, add(x, r, regenv1), e);
			concat(e1dash, (r, t), (e2dash, regenv2))
		case Forget(x, e) => assert(false)
	}

	// 使用される変数をスタックからレジスタへRestore (caml2html: regalloc_unspill)
	def gdash_and_restore(dest, cont, regenv, exp) = {
		try {
			gdash(dest, cont, regenv, exp)
		} catch {
		NoReg(x, t) =>
			// println("restoring " + x + "@.")
			g(dest, cont, regenv, Let((x, t), Restore(x), Ans(exp))))
		}
	}

	// 各命令のレジスタ割り当て (caml2html: regalloc_gprime)
	def gdash(dest, cont, regenv, e) = e match {
		case Nop() | Set(_) | SetL(_) | Comment(_) | Restore(_) => (Ans(e), regenv)
		case Mov(x) => (Ans(Mov(find(x, Type.Int(), regenv))), regenv)
		case Neg(x) => (Ans(Neg(find(x, Type.Int(), regenv))), regenv)
		case Add(x, ydash) => (Ans(Add(find(x, Type.Int(), regenv), finddash(ydash, regenv))), regenv)
		case Sub(x, ydash) => (Ans(Sub(find(x, Type.Int(), regenv), finddash(ydash, regenv))), regenv)
		case SLL(x, ydash) => (Ans(SLL(find(x, Type.Int(), regenv), finddash(ydash, regenv))), regenv)
		case Ld (x, ydash) => (Ans(Ld (find(x, Type.Int(), regenv), finddash(ydash, regenv))), regenv)
		case St(x, y, zdash) => (Ans(St(find(x, Type.Int(), regenv), find(y, Type.Int(), regenv), finddash(zdash, regenv))), regenv)
		case FMovD(x) => (Ans(FMovD(find(x, Type.Float(), regenv))), regenv)
		case FNegD(x) => (Ans(FNegD(find(x, Type.Float(), regenv))), regenv)
		case FAddD(x, y) => (Ans(FAddD(find(x, Type.Float(), regenv), find(y, Type.Float(), regenv))), regenv)
		case FSubD(x, y) => (Ans(FSubD(find(x, Type.Float(), regenv), find(y, Type.Float(), regenv))), regenv)
		case FMulD(x, y) => (Ans(FMulD(find(x, Type.Float(), regenv), find(y, Type.Float(), regenv))), regenv)
		case FDivD(x, y) => (Ans(FDivD(find(x, Type.Float(), regenv), find(y, Type.Float(), regenv))), regenv)
		case LdDF(x, ydash) => (Ans(LdDF(find(x, Type.Int(), regenv), finddash(ydash, regenv))), regenv)
		case StDF(x, y, zdash) => (Ans(StDF(find(x, Type.Float(), regenv), find(y, Type.Int(), regenv), finddash(zdash, regenv))), regenv)
		case exp@IfEq(x, ydash, e1, e2) => gdash_if(dest, cont, regenv, exp, (e1dash, e2dash) => IfEq (find(x, Type.Int(),   regenv), finddash(ydash, regenv), e1dash, e2dash), e1, e2)
		case exp@IfLE(x, ydash, e1, e2) => gdash_if(dest, cont, regenv, exp, (e1dash, e2dash) => IfLE (find(x, Type.Int(),   regenv), finddash(ydash, regenv), e1dash, e2dash), e1, e2)
		case exp@IfGE(x, ydash, e1, e2) => gdash_if(dest, cont, regenv, exp, (e1dash, e2dash) => IfGE (find(x, Type.Int(),   regenv), finddash(ydash, regenv), e1dash, e2dash), e1, e2)
		case exp@IfFEq(x, y, e1, e2)    => gdash_if(dest, cont, regenv, exp, (e1dash, e2dash) => IfFEq(find(x, Type.Float(), regenv), find(y, Type.Float(), regenv), e1dash, e2dash), e1, e2)
		case exp@IfFLE(x, y, e1, e2)    => gdash_if(dest, cont, regenv, exp, (e1dash, e2dash) => IfFLE(find(x, Type.Float(), regenv), find(y, Type.Float(), regenv), e1dash, e2dash), e1, e2)
		case exp@CallCls(x, ys, zs)   => gdash_call(dest, cont, regenv, exp, (ys, zs) => CallCls(find(x, Type.Int(), regenv), ys, zs), ys, zs)
		case exp@CallDir(l, ys, zs)   => gdash_call(dest, cont, regenv, exp, (ys, zs) => CallDir(l, ys, zs), ys, zs)
		case Save(x, y) => assert(false)
	}

	// ifのレジスタ割り当て (caml2html: regalloc_if)
	def gdash_if(dest, cont, regenv, exp, constr, e1, e2) = {
		val (e1dash, regenv1) = g(dest, cont, regenv, e1);
		val (e2dash, regenv2) = g(dest, cont, regenv, e2);
		
		// 両方に共通のレジスタ変数だけ利用
		val regenvdash = List.fold_left(
			(regenvdash, x) => {
				try {
					if (is_reg(x)) {
						regenvdash
					} else {
						val r1 = M.find(x, regenv1);
						val r2 = M.find(x, regenv2);
						if (r1 != r2) {
							regenvdash
						} else {
							M.add(x, r1, regenvdash)
						}
					}
				} catch {
				case Not_found => regenvdash
				}
			}
			M.empty,
			fv(cont)
		);
		
		List.fold_left(
			(e, x) => {
				if (x == fst(dest) || not(M.mem(x, regenv)) || M.mem(x, regenvdash)) {
					e
				} else {
					 seq(Save(M.find(x, regenv), x), e)
				}
			}, // そうでない変数は分岐直前にセーブ
			Ans(constr(e1dash, e2dash)),
			fv(cont),
			regenvdash
		 )
	}
	// 関数呼び出しのレジスタ割り当て (caml2html: regalloc_call)
	def gdash_call(dest, cont, regenv, exp, constr, ys, zs) = {
		List.fold_left(
			(e, x) => {
				if (x == fst(dest) || not(M.mem(x, regenv))) {
					e
				} else {
					seq(Save(M.find(x, regenv), x), e))
				}
			},
			Ans(constr(
				List.map(y => find(y, Type.Int(),   regenv), ys),
				List.map(z => find(z, Type.Float(), regenv), zs)
			)),
			fv(cont),
			M.empty()
		)
	}

	// 関数のレジスタ割り当て (caml2html: regalloc_h)
	def h { name = Id.L(x); args = ys; fargs = zs; body = e; ret = t } = {
		val regenv = M.add(x, reg_cl, M.empty);
		val (i, arg_regs, regenv) = List.fold_left(
			((i, arg_regs, regenv), y) => {
				val r = regs.(i);
				(i + 1, arg_regs ::: List(r), { assert(not(is_reg(y))); M.add(y, r, regenv) })
			},
			(0, List(), regenv),
			ys
		);
		val (d, farg_regs, regenv) = List.fold_left (
			((d, farg_regs, regenv), z) => {
				val fr = fregs.(d);
				(d + 1, farg_regs ::: List(fr), { assert(not(is_reg(z))); M.add(z, fr, regenv) } )
			},
			(0, List(), regenv),
			zs
		);
		val a = t match {
			case Type.Unit() => Id.gentmp(Type.Unit())
			case Type.Float() => fregs.(0)
			case _ => regs.(0)
		};
		val (edash, regenvdash) = g((a, t), Ans(Mov(a)), regenv, e);
		
		{ name = Id.L(x); args = arg_regs; fargs = farg_regs; body = edash; ret = t }
	}

	// プログラム全体のレジスタ割り当て (caml2html: regalloc_f)
	def f (e:Prog):Prog = e match {
	case Prog(data, fundefs, e) =>
		println("register allocation: may take some time (up to a few minutes, depending on the size of functions)@.");
		val fundefsdash = fundefs.map(h);
		val (edash, regenvdash) = g((Id.gentmp(Type.Unit()), Type.Unit()), (Ans(Nop())), M.empty, e);
		Prog(data, fundefsdash, edash)
	}
}
