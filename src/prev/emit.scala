open SparcAsm

def gethi(a:double):int = {}
def getlo(a:double):int = {}



var stackset = S.empty() // すでにSaveされた変数の集合 (caml2html: emit_stackset)
var stackmap = List() // Saveされた変数の、スタックにおける位置 (caml2html: emit_stackmap)

def save(x) = {
	stackset = S.add(x, stackset);
	if (not(List.mem(x, stackmap))) {
		stackmap = stackmap ::: List(x)
	}
}

def savef(x) = {
	stackset = S.add(x, stackset);
	if (not (List.mem(x, stackmap))) {
		val pad = if(List.length(stackmap) % 2 == 0) List() else List(Id.gentmp(Type.Int()))
		stackmap = stackmap ::: pad ::: List(x, x)
	}
}

def locate(x) = {
	def loc(e) = e match {
		case List() => List()
		case y :: zs if (x == y) => 0 :: List.map(succ, loc(zs))
		case y :: zs             =>      List.map(succ, loc(zs))
	}
	loc(stackmap)
}

def offset(x) = 4 * List.hd(locate(x))

def stacksize() = align( (List.length(stackmap) + 1) * 4)

def pp_id_or_imm(e) = e match {
	case V(x) => x
	case C(i) => string_of_int(i)
}

// 関数呼び出しのために引数を並べ替える(register shuffling) (caml2html: emit_shuffle)
def shuffle(sw, xys) = {
	// remove identical moves
	val (_, xys) = List.partition((x, y) => x == y, xys);

	// find acyclic moves
	List.partition((_, y) => List.mem_assoc(y, xys), xys) match {
		case (List(), List()) => List()
		case ((x, y) :: xys, List()) => {// no acyclic moves; resolve a cyclic move
				(y, sw) :: (x, y) :: shuffle( sw, List.map(
					e => e match {
						case (ydash, z) if (y == ydash) => (sw, z)
						case yz => yz
					},
					xys
				))
			}
		case (xys, acyc) => acyc ::: shuffle(sw, xys)
	}
}

abstract sealed class Dest
case class Tail() extends Dest
case class NonTail(a:Id.t) extends Dest // 末尾かどうかを表すデータ型 (caml2html: emit_dest)

// 命令列のアセンブリ生成 (caml2html: emit_g)
def g(oc, e) = e match { 
	case (dest, Ans(exp))            => gdash(oc, (dest, exp))
	case (dest, Let((x, t), exp, e)) => gdash(oc, (NonTail(x), exp)); g(oc, (dest, e))
	case (_, Forget(_))              => assert(false)
}

// 各命令のアセンブリ生成 (caml2html: emit_gprime)
def gdash(oc, e):Unit = e match {
	// 末尾でなかったら計算結果をdestにセット (caml2html: emit_nontail)
	case (NonTail(_), Nop()) =>
	case (NonTail(x), Set(i)) => oc.print("\tset\t"+i+", "+x+"\n")
	case (NonTail(x), SetL(Id.L(y))) => oc.print("\tset\t"+y+", "+x+"\n")
	case (NonTail(x), Mov(y)) if (x == y) =>
	case (NonTail(x), Mov(y)) => oc.print("\tmov\t"+y+", "+x+"\n")
	case (NonTail(x), Neg(y)) => oc.print("\tneg\t"+y+", "+x+"\n")
	case (NonTail(x), Add(y, zdash)) => oc.print("\tadd\t"+y+", "+pp_id_or_imm(zdash)+", "+x+"\n")
	case (NonTail(x), Sub(y, zdash)) => oc.print("\tsub\t"+y+", "+pp_id_or_imm(zdash)+", "+x+"\n")
	case (NonTail(x), SLL(y, zdash)) => oc.print("\tsll\t"+y+", "+pp_id_or_imm(zdash)+", "+x+"\n")
	case (NonTail(x), Ld(y, zdash)) => oc.print("\tld\t["+y+" + "+pp_id_or_imm(zdash)+"], "+x+"\n")
	case (NonTail(_), St(x, y, zdash)) => oc.print("\tst\t"+x+", ["+y+" + "+pp_id_or_imm(zdash)+"]\n")
	case (NonTail(x), FMovD(y)) if (x == y) =>
	case (NonTail(x), FMovD(y)) =>
		oc.print("\tfmovs\t"+y+", "+x+"\n");
		oc.print("\tfmovs\t"+co_freg(y)+", "+co_freg(x)+"\n");
	case (NonTail(x), FNegD(y)) =>
		oc.print("\tfnegs\t"+y+", "+x+"\n");
		if (x != y) oc.print("\tfmovs\t"+co_freg(y)+", "+co_freg(x)+"\n")
	case (NonTail(x), FAddD(y, z)) => oc.print("\tfaddd\t"+y+", "+z+", "+x+"\n")
	case (NonTail(x), FSubD(y, z)) => oc.print("\tfsubd\t"+y+", "+z+", "+x+"\n")
	case (NonTail(x), FMulD(y, z)) => oc.print("\tfmuld\t"+y+", "+z+", "+x+"\n")
	case (NonTail(x), FDivD(y, z)) => oc.print("\tfdivd\t"+y+", "+z+", "+x+"\n")
	case (NonTail(x), LdDF(y, zdash)) => oc.print("\tldd\t["+y+" + "+pp_id_or_imm(zdash)+"], "+x+"\n")
	case (NonTail(_), StDF(x, y, zdash)) => oc.print("\tstd\t"+x+", ["+y+" + "+pp_id_or_imm(zdash)+"]\n")
	case (NonTail(_), Comment(s)) => oc.print("\t! "+s+"\n")
	// 退避の仮想命令の実装 (caml2html: emit_save)
	case (NonTail(_), Save(x, y)) if (List.mem(x, allregs) && not(S.mem(y, stackset))) =>
		save(y);
		oc.print("\tst\t"+x+", ["+reg_sp+" + "+offset(y)+"]\n")
	case (NonTail(_), Save(x, y)) if (List.mem(x, allfregs) && not(S.mem(y, stackset))) =>
		savef(y);
		oc.print("\tstd\t"+x+", ["+reg_sp+" + "+offset(y)+"]\n")
	case (NonTail(_), Save(x, y)) => assert(S.mem(y, stackset)); Unit
	// 復帰の仮想命令の実装 (caml2html: emit_restore)
	case (NonTail(x), Restore(y)) if (List.mem(x, allregs)) =>
		oc.print("\tld\t["+reg_sp+" + "+offset(y)+"], "+x+"\n")
	case (NonTail(x), Restore(y)) =>
		assert(List.mem(x, allfregs));
		oc.print("\tldd\t["+reg_sp+" + "+offset(y)+"], "+x+"\n")
	// 末尾だったら計算結果を第一レジスタにセットしてret (caml2html: emit_tailret)
	case (Tail(), exp@(Nop() | St(_) | StDF(_) | Comment(_) | Save(_))) =>
		gdash(oc, (NonTail(Id.gentmp(Type.Unit())), exp));
		oc.print("\tretl\n");
		oc.print("\tnop\n")
	case (Tail, exp@(Set(_) | SetL (_) | Mov(_) | Neg(_) | Add(_) | Sub(_) | SLL(_) | Ld(_))) =>
		gdash(oc, (NonTail(regs.(0)), exp));
		oc.print("\tretl\n");
		oc.print("\tnop\n")
	case (Tail, exp@(FMovD(_) | FNegD(_) | FAddD(_) | FSubD(_) | FMulD(_) | FDivD(_) | LdDF(_))) =>
		gdash(oc, (NonTail(fregs.(0)), exp));
		oc.print("\tretl\n");
		oc.print("\tnop\n")
	case (Tail, exp@Restore(x)) =>
		locate(x) match {
			case List(i) => gdash(oc, (NonTail(regs.(0)), exp))
			case List(i, j) if (i + 1 == j) => gdash(oc, (NonTail(fregs.(0)), exp))
			case _ => assert(false)
		}
		oc.print("\tretl\n");
		oc.print("\tnop\n")
	case (Tail, IfEq(x, ydash, e1, e2)) =>
		oc.print("\tcmp\t"+x+", "+pp_id_or_imm(ydash)+"\n");
		gdash_tail_if(oc, e1, e2, "be", "bne")
	case (Tail, IfLE(x, ydash, e1, e2)) =>
		oc.print("\tcmp\t"+x+", "+pp_id_or_imm(ydash)+"\n");
		gdash_tail_if(oc, e1, e2, "ble", "bg")
	case (Tail, IfGE(x, ydash, e1, e2)) =>
		oc.print("\tcmp\t"+x+", "+pp_id_or_imm(ydash)+"\n");
		gdash_tail_if(oc, e1, e2, "bge", "bl")
	case (Tail, IfFEq(x, y, e1, e2)) =>
		oc.print("\tfcmpd\t"+x+", "+y+"\n");
		oc.print("\tnop\n");
		gdash_tail_if(oc, e1, e2, "fbe", "fbne")
	case (Tail, IfFLE(x, y, e1, e2)) =>
		oc.print("\tfcmpd\t"+x+", "+y+"\n");
		oc.print("\tnop\n");
		gdash_tail_if(oc, e1, e2, "fble", "fbg")
	case (NonTail(z), IfEq(x, ydash, e1, e2)) =>
		oc.print("\tcmp\t"+x+", "+pp_id_or_imm(ydash)+"\n");
		gdash_non_tail_if(oc, NonTail(z), e1, e2, "be", "bne")
	case (NonTail(z), IfLE(x, ydash, e1, e2)) =>
		oc.print("\tcmp\t"+x+", "+pp_id_or_imm(ydash)+"\n");
		gdash_non_tail_if(oc, NonTail(z), e1, e2, "ble", "bg")
	case (NonTail(z), IfGE(x, ydash, e1, e2)) =>
		oc.print("\tcmp\t"+x+", "+pp_id_or_imm(ydash)+"\n");
		gdash_non_tail_if(oc, NonTail(z), e1, e2, "bge", "bl")
	case (NonTail(z), IfFEq(x, y, e1, e2)) =>
		oc.print("\tfcmpd\t"+x+", "+y+"\n");
		oc.print("\tnop\n");
		gdash_non_tail_if(oc, NonTail(z), e1, e2, "fbe", "fbne")
	case (NonTail(z), IfFLE(x, y, e1, e2)) =>
		oc.print("\tfcmpd\t"+x+", "+y+"\n");
		oc.print("\tnop\n");
		gdash_non_tail_if(oc, NonTail(z), e1, e2, "fble", "fbg")
	// 関数呼び出しの仮想命令の実装 (caml2html: emit_call)
	case (Tail, CallCls(x, ys, zs)) => // 末尾呼び出し (caml2html: emit_tailcall)
		gdash_args(oc, List((x, reg_cl)), ys, zs);
		oc.print("\tld\t["+reg_cl+" + 0], "+reg_sw+"\n");
		oc.print("\tjmp\t"+reg_sw+"\n");
		oc.print("\tnop\n")
	case (Tail, CallDir(Id.L(x), ys, zs)) => // 末尾呼び出し
		gdash_args(oc, List(), ys, zs);
		oc.print("\tb\t"+x+"\n");
		oc.print("\tnop\n")
	case (NonTail(a), CallCls(x, ys, zs)) =>
		gdash_args(oc, List((x, reg_cl)), ys, zs);
		val ss = stacksize();
		oc.print("\tst\t"+reg_ra+", ["+reg_sp+" + "+(ss - 4)+"]\n");
		oc.print("\tld\t["+reg_cl+" + 0], "+reg_sw+"\n");
		oc.print("\tcall\t"+reg_sw+"\n");
		oc.print("\tadd\t"+reg_sp+", "+ss+", "+reg_sp+"\t! delay slot\n");
		oc.print("\tsub\t"+reg_sp+", "+ss+", "+reg_sp+"\n");
		oc.print("\tld\t["+reg_sp+" + "+(ss - 4)+"], "+reg_ra+"\n");
		if (List.mem(a, allregs) && a != regs(0)) {
			oc.print("\tmov\t"+regs(0)+", "+a+"\n")
		} else if (List.mem(a, allfregs) && a != fregs(0)) {
			 oc.print("\tfmovs\t"+fregs(0)+", "+a+"\n");
			 oc.print("\tfmovs\t"+co_freg(fregs(0))+", "+co_freg(a)+"\n")
		}
	case (NonTail(a), CallDir(Id.L(x), ys, zs)) =>
		gdash_args(oc, List(), ys, zs);
		val ss = stacksize ();
		oc.print("\tst\t"+reg_ra+", ["+reg_sp+" + "+(ss - 4)+"]\n");
		oc.print("\tcall\t"+x+"\n");
		oc.print("\tadd\t"+reg_sp+", "+ss+", "+reg_sp+"\t! delay slot\n");
		oc.print("\tsub\t"+reg_sp+", "+ss+", "+reg_sp+"\n");
		oc.print("\tld\t["+reg_sp+" + "+(ss - 4)+"], "+reg_ra+"\n");
		if (List.mem(a, allregs) && a != regs(0)) {
			oc.print("\tmov\t"+regs(0)+", "+a+"\n")
		} else if (List.mem(a, allfregs) && a != fregs(0)) {
			oc.print("\tfmovs\t"+fregs(0)+", "+a+"\n");
			oc.print("\tfmovs\t"+co_freg(fregs(0))+", "+co_freg(a)+"\n")
		}
}

def gdash_tail_if(oc, e1, e2, b, bn) = {
	val b_else = Id.genid(b + "_else");

	oc.print("\t"+bn+"\t"+b_else+"\n");
	oc.print("\tnop\n");

	val stackset_back = stackset;

	g(oc, (Tail, e1));
	oc.print(b_else+":\n");
	stackset = stackset_back;
	g(oc, (Tail, e2))
}

def gdash_non_tail_if(oc, dest, e1, e2, b, bn) = {
	val b_else = Id.genid(b + "_else");
	val b_cont = Id.genid(b + "_cont");

	oc.print("\t"+bn+"\t"+b_else+"\n");
	oc.print("\tnop\n");
	val stackset_back = stackset;

	g(oc, (dest, e1));

	val stackset1 = stackset;

	oc.print("\tb\t"+b_cont+"\n");
	oc.print("\tnop\n");
	oc.print(""+b_else+":\n");

	stackset = stackset_back;

	g(oc, (dest, e2));

	oc.print(""+b_cont+":\n");

	val stackset2 = stackset;
	stackset = S.inter(stackset1, stackset2)
}

def gdash_args(oc, x_reg_cl, ys, zs) = {
	val (i, yrs) = List.fold_left(
			((i, yrs), y) => (i + 1, (y, regs.(i)) :: yrs),
			(0, x_reg_cl),
			ys
	);
	List.iter(
		(y, r) => oc.print("\tmov\t"+y+", "+r+"\n"),
		shuffle(reg_sw,yrs)
	);
	val (d, zfrs) = List.fold_left(
			((d, zfrs), z) => (d + 1, (z, fregs.(d)) :: zfrs),
			(0, List()),
			zs
	);
	List.iter(
		(z, fr) => {
			oc.print("\tfmovs\t"+z+", "+fr+"\n");
			oc.print("\tfmovs\t"+co_freg(z)+", "+co_freg(fr)+"\n");
		},
		shuffle(reg_fsw, zfrs)
	);
}

def h(oc, { name = Id.L(x); args = _; fargs = _; body = e; ret = _ }) = {
	oc.print(x + ":\n")
	stackset = S.empty();
	stackmap = List();
	g(oc, (Tail, e))
}

def f(oc, (Prog(data, fundefs, e))) = {
	println("generating assembly...@.");
	oc.print(".section\t\".rodata\"\n");
	oc.print(".align\t8\n");
	List.iter(
		(Id.L(x), d) => {
			oc.print(x + ":\t! "+d+"\n");
			oc.print("\t.long\t0x"+gethi(d)+"\n");
			oc.print("\t.long\t0x"+getlo(d)+"\n");
		},
		data
	)
	oc.print(".section\t\".text\"\n");
	List.iter(fundef => h(oc, fundef), fundefs);
	oc.print(".global\tmin_caml_start\n");
	oc.print("min_caml_start:\n");
	oc.print("\tsave\t%sp, -112, %sp\n"); // from gcc; why 112?
	stackset = S.empty();
	stackmap = List();
	g(oc, (NonTail("%g0"), e));
	oc.print("\tret\n");
	oc.print("\trestore\n")
}
