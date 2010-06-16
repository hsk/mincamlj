package mincaml;
import scala.collection.immutable._;

object RegAlloc extends X86Asm {
	type Selt = Id.T;
	
	// allocate a register or fail
	// val alloc : X86Asm.t -> S.elt M.t -> M.key -> Type.t -> M.key = <fun>
/*	def alloc(cont:T, regenv:Map[Id.T,Selt], x:Id.T, t:Type.T):Id.T = {
		if (!regenv.contains(x)) throw new Exception();
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
					free.foldLeft(Set()){
					case (live, y) =>
						if (is_reg(y)) {
							live + y
						} else {
							try {
								live + regenv(y)
							} catch {
							case _ => live
							}
						}
					};
				val r = // そうでないレジスタを探す
					all.find{case r => !live.contains(r)};
				// println("allocated " + x + " to " + r + "@.");
				r
			} catch {
			case _ => throw new Exception("register allocation failed for " + x)
			}
		}
	}

	// auxiliary function for g and gdash_and_restore
	// val add : Id.t -> Id.t -> Id.t M.t -> Id.t M.t =
	def add(x:Id.T, r:Id.T, regenv:Map[Id.T,Id.T]):Map[Id.T,Id.T] = {
		if (is_reg(x)) {
			if (x == r) throw new Exception();
			regenv
		} else {
			regenv + (x -> r)
		}
	}
*/
	// auxiliary functions for gdash
	case class NoReg(a:Id.T, b:Type.T) extends Exception;
	// val find : Id.t -> Type.t -> Id.t M.t -> Id.t = <fun>
	def find(x:Id.T, t:Type.T, regenv:Map[Id.T,Id.T]):Id.T = {
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
/*	
	// val find' : X86Asm.id_or_imm -> Id.t M.t -> X86Asm.id_or_imm
	def finddash(xdash:id_or_imm, regenv:Map[Id.T,Id.T]):id_or_imm = xdash match {
		case V(x) => V(regenv((x, Type.Int())))
		case c => c
	}
*/
	// Id.t * Type.t -> X86Asm.t -> Id.t M.t -> X86Asm.t -> X86Asm.t * S.elt M.t
	// 命令列のレジスタ割り当て (caml2html: regalloc_g)
	def g(dest:(Id.T, Type.T), cont:T, regenv:Map[Id.T, Id.T], e:T):(T,Map[Id.T,Selt]) = e match {
		case Ans(exp) => gdash_and_restore(dest, cont, regenv, exp);
		case _ => throw new Exception();
/*
		case Let(xt@(x, t), exp, e) =>
			if(regenv.contains(x))throw new Exception();
			val contdash = concat(e, dest, cont);
			val (e1dash, regenv1) = gdash_and_restore(xt, contdash, regenv, exp);
			val r = alloc(contdash, regenv1, x, t);
			val (e2dash, regenv2) = g(dest, cont, add(x, r, regenv1), e);
			concat(e1dash, (r, t), (e2dash, regenv2))
		case Forget(x, e) => throw new Exception()
*/		
	}
	// val g'_and_restore :
	//  Id.t * Type.t -> X86Asm.t -> Id.t M.t -> X86Asm.exp -> X86Asm.t * S.elt M.t
	// 使用される変数をスタックからレジスタへRestore (caml2html: regalloc_unspill)
	def gdash_and_restore(dest:(Id.T, Type.T), cont:T, regenv:Map[Id.T, Id.T], exp:Exp):(T, Map[Id.T,Selt]) = {
		try {
			gdash(dest, cont, regenv, exp)
		} catch {
		case NoReg(x, t) =>
			// println("restoring " + x + "@.")
			g(dest, cont, regenv, Let((x, t), Restore(x), Ans(exp)))
		}
	}

	//  Id.t * Type.t -> X86Asm.t -> Id.t M.t -> X86Asm.exp -> X86Asm.t * S.elt M.t =
	// 各命令のレジスタ割り当て (caml2html: regalloc_gprime)
	def gdash(dest:(Id.T,Type.T), cont:T, regenv:Map[Id.T, Id.T], e:Exp):(T,Map[Id.T, Selt]) = e match {
		case Nop() | SET(_) | SETL(_) | Comment(_) | Restore(_) => (Ans(e), regenv)
		case Mov(x) => (Ans(Mov(find(x, Type.Int(), regenv))), regenv)
		case _ => (Ans(e), regenv)/*
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
*/
	}
/*

	//  Id.t * Type.t ->
	//  X86Asm.t ->
	//  Id.t M.t ->
	//  X86Asm.exp ->
	//  (X86Asm.t -> X86Asm.t -> X86Asm.exp) ->
	//  X86Asm.t -> X86Asm.t -> X86Asm.t * S.elt M.t = <fun>
  
	// ifのレジスタ割り当て (caml2html: regalloc_if)
	def gdash_if(
		dest:(Id.T, Type.T),
		cont:T,
		regenv:Map[Id.T,Selt],
		exp:Exp,
		constr:(T,T)=>Exp,
		e1:T, e2:T):(T, Set[Type]) = {
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
			Map(),
			fv(cont)
		);
		
		List.fold_left(
			(e, x) => {
				if (x == fst(dest) || not(M.mem(x, regenv)) || M.mem(x, regenvdash)) {
					e
				} else {
					 seq(Save(regenv(x), x), e)
				}
			}, // そうでない変数は分岐直前にセーブ
			Ans(constr(e1dash, e2dash)),
			fv(cont),
			regenvdash
		 )
	}

	//  Id.t * Type.t ->
	//  X86Asm.t ->
	//  Id.t M.t ->
	//  X86Asm.exp ->
	//  (Id.t list -> Id.t list -> X86Asm.exp) ->
	//  Id.t list -> Id.t list -> X86Asm.t * S.elt M.t = <fun>
	// 関数呼び出しのレジスタ割り当て (caml2html: regalloc_call)
	def gdash_call(
		dest:(Id.T, Type.T),
		cont:T,
		regenv:Map[Id.T,Id.T],
		exp:Exp, 
		constr:(List[Id.T],List[Id.T])=>Exp,
		ys:List[Id.T],
		zs:List[Id.T]
	):(T,Map[Id.T,Selt]) = {
		List.fold_left(
			(e, x) => {
				if (x == fst(dest) || not(M.mem(x, regenv))) {
					e
				} else {
					seq(Save(regenv(x), x), e)
				}
			},
			Ans(constr(
				ys.map{case y => find(y, Type.Int(),   regenv)},
				zs.map{case z => find(z, Type.Float(), regenv)}
			)),
			fv(cont),
			M.empty()
		)
	}
*/

	// X86Asm.fundef -> X86Asm.fundef
	// 関数のレジスタ割り当て (caml2html: regalloc_h)
	def h(f:Fundef):Fundef = f match {
		case Fundef(x, ys, zs, e, t) =>
			val regenv0 = Map(x -> reg_cl);
			val (i, arg_regs, regenv1) = ys.foldLeft((0, List[Id.T](), regenv0)) {
				case ((i, arg_regs, regenv), y) =>
					val r = regs(i);
					(
						i + 1,
						arg_regs ::: List(r),
						{ if(is_reg(y))throw new Exception(); regenv + (y -> r) }
					)
			};

			val (d, farg_regs, regenv2) = zs.foldLeft ((0, List[Id.T](), regenv1)) {
				case ((d, farg_regs, regenv), z) =>
					val fr = fregs(d);
					(d + 1, farg_regs ::: List(fr), { if(is_reg(z))throw new Exception(); regenv + (z -> fr) } )
			}
			val a:Id.T = t match {
				case Type.Unit() => Id.gentmp(Type.Unit())
				case Type.Float() => fregs(0)
				case _ => regs(0)
			};
			val (edash:T, regenvdash) = g((a, t), Ans(Mov(a)), regenv2, e);
			
			Fundef(x, arg_regs, farg_regs, edash, t)
	}

	// プログラム全体のレジスタ割り当て (caml2html: regalloc_f)
	def f1(e:Prog):Prog = e match {
		case Prog(data, fundefs, e) =>
/*			println("register allocation: may take some time (up to a few minutes, depending on the size of functions)@.");
			val fundefsdash = fundefs.map(h);
			val (edash, regenvdash) = g((Id.gentmp(Type.Unit()), Type.Unit()), (Ans(Nop())), Map(), e);
			Prog(data, fundefsdash, edash)*/
			null
	}

	def f(e:X86Asm.Prog):X86Asm.Prog = f1(e.asInstanceOf[Prog]).asInstanceOf[X86Asm.Prog]
}
