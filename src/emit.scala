package mincaml;
import java.io._;
import scala.collection.immutable._;


object Emit extends X86Asm {
	type out_channel = PrintWriter;
	
	def gethi(a:Double):Int = {
		val l = java.lang.Double.doubleToLongBits(a);
		return (l>>32).asInstanceOf[Int];
	}
	def getlo(a:Double):Int = {
		java.lang.Double.doubleToLongBits(a).asInstanceOf[Int];
	}

	type Selt = Id.T
	var stackset = Set[Selt]() // すでにSaveされた変数の集合 (caml2html: emit_stackset)
	var stackmap = List[Selt]() // Saveされた変数の、スタックにおける位置 (caml2html: emit_stackmap)

	def save(x:Selt) = {
		stackset = stackset + x;
		if (!(stackmap.contains(x))) {
			stackmap = stackmap ::: List(x)
		}
	}

	def savef(x:Selt){
		stackset = stackset + x;
		if (!stackmap.contains(x)) {
			val pad:List[Selt] = if(stackmap.size % 2 == 0) List[Selt]() else List[Selt](Id.gentmp(Type.Int()))
			stackmap = stackmap ::: pad ::: List(x, x)
		}
	}

	// val locate : S.elt -> int list
	
	def locate(x:Selt):List[Int] = {
		def loc(e:List[Selt]):List[Int] = e match {
			case List() => List()
			case y :: zs if (x == y) => 0 :: loc(zs).map{_ + 1}
			case y :: zs             =>      loc(zs).map{_ + 1}
		}
		loc(stackmap)
	}
	def offset(x:Selt):Int = 4 * (locate(x) match {case x::xs => x;case _ => return 0;})

	def stacksize():Int = align( (stackmap.size + 1) * 4)

	def pp_id_or_imm(e:id_or_imm):Id.T = e match {
		case V(x) => x
		case C(i) => ""+i
	}

	// val shuffle : 'a -> ('a * 'a) list -> ('a * 'a) list = <fun>
	type A = Id.T
	// 関数呼び出しのために引数を並べ替える(register shuffling) (caml2html: emit_shuffle)
	def shuffle(sw:A, xys:List[(A, A)]):List[(A, A)] = {
		// remove identical moves
		val (_, xys2) = xys.partition{case (x, y) => x == y};
		val xysh = Map[A,A]() ++ xys2;
		// find acyclic moves
		xys2.partition{case (_, y) => xysh.contains(y)} match {
			case (List(), List()) => List()
			case ((x, y) :: xys, List()) => {// no acyclic moves; resolve a cyclic move
					(y, sw) :: (x, y) :: shuffle( sw, xys2.map {
							case (ydash, z) if (y == ydash) => (sw, z)
							case yz => yz
						}
					)
				}
			case (xys3, acyc) => acyc ::: shuffle(sw, xys3)
		}
	}

	abstract sealed class Dest
	case class Tail() extends Dest
	case class NonTail(a:Id.T) extends Dest // 末尾かどうかを表すデータ型 (caml2html: emit_dest)


	// val g : out_channel -> dest * X86Asm.t -> unit = <fun>
  
	// 命令列のアセンブリ生成 (caml2html: emit_g)
	def g(oc:out_channel, e:(Dest, T)):Unit = e match { 
		case (dest, Ans(exp))            => gdash(oc, (dest, exp))
		case (dest, Let((x, t), exp, e)) => gdash(oc, (NonTail(x), exp)); g(oc, (dest, e))
		case (_, Forget(_, _))           => throw new Exception()
	}

	// val g' : out_channel -> dest * X86Asm.exp -> unit = <fun>

	// 各命令のアセンブリ生成 (caml2html: emit_gprime)
	def gdash(oc:out_channel, e:(Dest,Exp)):Unit = e match {
		// 末尾でなかったら計算結果をdestにセット (caml2html: emit_nontail)
		case (NonTail(_), Nop()) =>
		case (NonTail(x), SET(i)) => oc.print("\tset\t"+i+", "+x+"\n")
		case (NonTail(x), SETL(y)) => oc.print("\tset\t"+y+", "+x+"\n")
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
		case (NonTail(_), Save(x, y)) if (allregs.contains(x) && !stackset.contains(y)) =>
			save(y);
			oc.print("\tst\t"+x+", ["+reg_sp+" + "+offset(y)+"]\n")
		case (NonTail(_), Save(x, y)) if (allfregs.contains(x) && !stackset.contains(y)) =>
			savef(y);
			oc.print("\tstd\t"+x+", ["+reg_sp+" + "+offset(y)+"]\n")
		case (NonTail(_), Save(x, y)) => if(!stackset.contains(y))throw new Exception()
		// 復帰の仮想命令の実装 (caml2html: emit_restore)
		case (NonTail(x), Restore(y)) if (allregs.contains(x)) =>
			oc.print("\tld\t["+reg_sp+" + "+offset(y)+"], "+x+"\n")
		case (NonTail(x), Restore(y)) =>
			if(!allfregs.contains(x))throw new Exception();
			oc.print("\tldd\t["+reg_sp+" + "+offset(y)+"], "+x+"\n")
		// 末尾だったら計算結果を第一レジスタにセットしてret (caml2html: emit_tailret)
		case (Tail(), exp@(Nop() | St(_, _, _) | StDF(_, _, _) | Comment(_) | Save(_, _))) =>
			gdash(oc, (NonTail(Id.gentmp(Type.Unit())), exp));
			oc.print("\tretl\n");
			oc.print("\tnop\n")
		case (Tail(), exp@(SET(_) | SETL (_) | Mov(_) | Neg(_) | Add(_, _) | Sub(_, _) | SLL(_, _) | Ld(_, _))) =>
			gdash(oc, (NonTail(regs(0)), exp));
			oc.print("\tretl\n");
			oc.print("\tnop\n")
		case (Tail(), exp@(FMovD(_) | FNegD(_) | FAddD(_, _) | FSubD(_, _) | FMulD(_, _) | FDivD(_, _) | LdDF(_, _))) =>
			gdash(oc, (NonTail(fregs(0)), exp));
			oc.print("\tretl\n");
			oc.print("\tnop\n")
		case (Tail(), exp@Restore(x)) =>
			locate(x) match {
				case List(i) => gdash(oc, (NonTail(regs(0)), exp))
				case (i:Int)::List(j) if ((i + 1) == j) => gdash(oc, (NonTail(fregs(0)), exp))
				case _ => throw new Exception()
			}
			oc.print("\tretl\n");
			oc.print("\tnop\n")
		case (Tail(), IfEq(x, ydash, e1, e2)) =>
			oc.print("\tcmp\t"+x+", "+pp_id_or_imm(ydash)+"\n");
			gdash_tail_if(oc, e1, e2, "be", "bne")
		case (Tail(), IfLE(x, ydash, e1, e2)) =>
			oc.print("\tcmp\t"+x+", "+pp_id_or_imm(ydash)+"\n");
			gdash_tail_if(oc, e1, e2, "ble", "bg")
		case (Tail(), IfGE(x, ydash, e1, e2)) =>
			oc.print("\tcmp\t"+x+", "+pp_id_or_imm(ydash)+"\n");
			gdash_tail_if(oc, e1, e2, "bge", "bl")
		case (Tail(), IfFEq(x, y, e1, e2)) =>
			oc.print("\tfcmpd\t"+x+", "+y+"\n");
			oc.print("\tnop\n");
			gdash_tail_if(oc, e1, e2, "fbe", "fbne")
		case (Tail(), IfFLE(x, y, e1, e2)) =>
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
		case (Tail(), CallCls(x, ys, zs)) => // 末尾呼び出し (caml2html: emit_tailcall)
			gdash_args(oc, List((x, reg_cl)), ys, zs);
			oc.print("\tld\t["+reg_cl+" + 0], "+reg_sw+"\n");
			oc.print("\tjmp\t"+reg_sw+"\n");
			oc.print("\tnop\n")
		case (Tail(), CallDir(x, ys, zs)) => // 末尾呼び出し
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
			if (allregs.contains(a) && a != regs(0)) {
				oc.print("\tmov\t"+regs(0)+", "+a+"\n")
			} else if (allfregs.contains(a) && a != fregs(0)) {
				 oc.print("\tfmovs\t"+fregs(0)+", "+a+"\n");
				 oc.print("\tfmovs\t"+co_freg(fregs(0))+", "+co_freg(a)+"\n")
			}
		case (NonTail(a), CallDir(x, ys, zs)) =>
			gdash_args(oc, List(), ys, zs);
			val ss = stacksize ();
			oc.print("\tst\t"+reg_ra+", ["+reg_sp+" + "+(ss - 4)+"]\n");
			oc.print("\tcall\t"+x+"\n");
			oc.print("\tadd\t"+reg_sp+", "+ss+", "+reg_sp+"\t! delay slot\n");
			oc.print("\tsub\t"+reg_sp+", "+ss+", "+reg_sp+"\n");
			oc.print("\tld\t["+reg_sp+" + "+(ss - 4)+"], "+reg_ra+"\n");
			if (allregs.contains(a) && a != regs(0)) {
				oc.print("\tmov\t"+regs(0)+", "+a+"\n")
			} else if (allregs.contains(a) && a != fregs(0)) {
				oc.print("\tfmovs\t"+fregs(0)+", "+a+"\n");
				oc.print("\tfmovs\t"+co_freg(fregs(0))+", "+co_freg(a)+"\n")
			}
	}

	//val g'_tail_if :
	// out_channel -> X86Asm.t -> X86Asm.t -> string -> string -> unit = <fun>

	def gdash_tail_if(oc:out_channel, e1:T, e2:T, b:String, bn:String) {
		val b_else = Id.genid(b + "_else");

		oc.print("\t"+bn+"\t"+b_else+"\n");
		oc.print("\tnop\n");

		val stackset_back = stackset;

		g(oc, (Tail(), e1));
		oc.print(b_else+":\n");
		stackset = stackset_back;
		g(oc, (Tail(), e2))
	}

	// val g'_non_tail_if :
	//   out_channel -> dest -> X86Asm.t -> X86Asm.t -> string -> string -> unit =
	//  <fun>

	def gdash_non_tail_if(oc:out_channel, dest:Dest, e1:T, e2:T, b:String, bn:String) {
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
		stackset = stackset1**stackset2;;
	}

	// val g'_args :
	//  out_channel -> (Id.t * Id.t) list -> Id.t list -> Id.t list -> unit = <fun>
	def gdash_args(oc:out_channel, x_reg_cl:List[(Id.T, Id.T)], ys:List[Id.T], zs:List[Id.T]) {

		val (i, yrs) = ys.foldLeft((0, x_reg_cl)) {
			case ((i, yrs), y) => (i + 1, (y, regs(i)) :: yrs)
		}

		shuffle(reg_sw, yrs).foreach {
			case (y, r) => oc.print("\tmov\t"+y+", "+r+"\n")
		}

		val (d, zfrs) = zs.foldLeft((0, List[(Id.T, String)]())){
			case ((d, zfrs), z) => (d + 1, (z, fregs(d)) :: zfrs)
		}

		shuffle(reg_fsw, zfrs).foreach {
			case (z, fr) =>
				oc.print("\tfmovs\t"+z+", "+fr+"\n");
				oc.print("\tfmovs\t"+co_freg(z)+", "+co_freg(fr)+"\n");
		}
	}

	// val h : out_channel -> X86Asm.fundef -> unit = <fun>
	def h(oc:out_channel, f:Fundef):Unit = f match {
		case Fundef(x, _, _, e, _) =>
			oc.print(x + ":\n");
			stackset = Set[Selt]();
			stackmap = List[Selt]();
			g(oc, (Tail(), e))
	}

	def f(oc:out_channel, p:X86Asm.Prog):Unit = p.asInstanceOf[Prog] match {
		case Prog(data, fundefs, e) =>
			println("generating assembly...@.");
			oc.print(".section\t\".rodata\"\n");
			oc.print(".align\t8\n");
			data.foreach {
				case (x, d) => 
					oc.print(x + ":\t! "+d+"\n");
					oc.print("\t.long\t0x"+gethi(d)+"\n");
					oc.print("\t.long\t0x"+getlo(d)+"\n");
			}
			oc.print(".section\t\".text\"\n");
			fundefs.foreach {
				case fundef => h(oc, fundef)
			}
			oc.print(".global\tmin_caml_start\n");
			oc.print("min_caml_start:\n");
			oc.print("\tsave\t%sp, -112, %sp\n"); // from gcc; why 112?
			stackset = Set[Selt]();
			stackmap = List[Selt]();
			g(oc, (NonTail("%g0"), e));
			oc.print("\tret\n");
			oc.print("\trestore\n")
	}
}
