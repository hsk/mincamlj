package mincaml;
import java.io._;
import scala.collection.immutable._;


object Emit_x86 extends X86Asm {
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
		case C(i) => "$" + i
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

	// val gdash : out_channel -> dest * X86Asm.exp -> unit = <fun>

	// 各命令のアセンブリ生成 (caml2html: emit_gprime)
	def gdash(oc:out_channel, e:(Dest,Exp)):Unit = e match {

	  // 末尾でなかったら計算結果をdestにセット (caml2html: emit_nontail)
		case (NonTail(_), Nop()) =>
		case (NonTail(x), SET(i)) => oc.println("\tmovl\t$"+i+", "+x)
		case (NonTail(x), SETL(y)) => oc.println("\tmovl\t$"+y+", "+x)
		case (NonTail(x), Mov(y)) if (x == y) =>
		case (NonTail(x), Mov(y)) => oc.println("\tmovl\t"+y+", "+x)
		case (NonTail(x), Neg(y)) =>
			if (x != y) {
				oc.println("\tmovl\t"+y+", "+x)
			}
			oc.println("\tnegl\t"+x)
		case (NonTail(x), Add(y, zdash)) =>
			if (x == y) {
				oc.println("\taddl\t"+pp_id_or_imm(zdash)+", "+x)
			} else if (x == pp_id_or_imm(zdash)) {
				oc.println("\taddl\t"+y+", "+x)
			} else {
				oc.println("\tmovl\t"+y+", "+x)
				oc.println("\taddl\t"+pp_id_or_imm(zdash)+", "+x)
			}
		case (NonTail(x), Sub(y, zdash)) =>
			if (x == y) {
				oc.println("\tsubl\t"+pp_id_or_imm(zdash)+", "+x)
			} else if (x == pp_id_or_imm(zdash)) {
				oc.println("\tsubl\t"+y+", "+x)
				oc.println("\tnegl\t"+x)
			} else {
				oc.println("\tmovl\t"+y+", "+x)
				oc.println("\tsubl\t"+pp_id_or_imm(zdash)+", "+x)
			}
		case (NonTail(x), SLL(y, zdash)) => oc.println("\tsll\t"+y+", "+pp_id_or_imm(zdash)+", "+x)
		case (NonTail(x), Ld(y, zdash)) =>
			zdash match {
				case V(x) => throw new Exception();
				case C(i) => oc.println("\tmovl\t"+i+"("+y+"), "+x)
			}
		case (NonTail(_), St(x, y, zdash)) =>
			zdash match {
				case V(x) => throw new Exception();
				case C(i) =>  oc.println("\tmovl\t"+x+", "+i+"("+y+")")
			}
		case (NonTail(x), FMovD(y)) if (x == y) =>
		case (NonTail(x), FMovD(y)) =>
			oc.println("\tfmovs\t"+y+", "+x);
			oc.println("\tfmovs\t"+co_freg(y)+", "+co_freg(x))
		case (NonTail(x), FNegD(y)) =>
			oc.println("\tfnegs\t"+y+", "+x);
			if (x != y) oc.println("\tfmovs\t"+co_freg(y)+", "+co_freg(x));
		case (NonTail(x), FAddD(y, z)) => oc.println("\tfaddd\t"+y+", "+z+", "+x)
		case (NonTail(x), FSubD(y, z)) => oc.println("\tfsubd\t"+y+", "+z+", "+x)
		case (NonTail(x), FMulD(y, z)) => oc.println("\tfmuld\t"+y+", "+z+", "+x)
		case (NonTail(x), FDivD(y, z)) => oc.println("\tfdivd\t"+y+", "+z+", "+x)
		case (NonTail(x), LdDF(y, zdash)) => oc.println("\tldd\t["+y+" + "+pp_id_or_imm(zdash)+"], "+x)
		case (NonTail(_), StDF(x, y, zdash)) => oc.println("\tstd\t"+x+", ["+y+" + "+pp_id_or_imm(zdash)+"]")
		case (NonTail(_), Comment(s)) => oc.println("\t! "+s) 
		// 退避の仮想命令の実装 (caml2html: emit_save)
		case (NonTail(_), Save(x, y)) if (allregs.contains(x) && !stackset.contains(y)) =>
			save(y);
			oc.println("\tmovl\t"+x+", "+offset(y)+"("+reg_sp+")")
		case (NonTail(_), Save(x, y)) if (allfregs.contains(x) && !stackset.contains(y)) =>
			savef(y);
			oc.println("\tstd\t"+x+", ["+reg_sp+" + "+offset(y)+"]");
		case (NonTail(_), Save(x, y)) => if(!stackset.contains(y)) throw new Exception();
		// 復帰の仮想命令の実装 (caml2html: emit_restore)
		case (NonTail(x), Restore(y)) if (allregs.contains(x)) =>
			oc.println("\tmovl\t"+offset(y)+"("+reg_sp+"), "+x)
		case (NonTail(x), Restore(y)) =>
			if(!allfregs.contains(x))throw new Exception();
			oc.println("\tldd\t["+reg_sp+" + "+offset(y)+"], "+x)
		// 末尾だったら計算結果を第一レジスタにセットしてret (caml2html: emit_tailret)
		case (Tail(), exp@(Nop() | St(_, _, _) | StDF(_, _, _) | Comment(_) | Save(_, _))) =>
			gdash(oc, (NonTail(Id.gentmp(Type.Unit())), exp));
			oc.println("\taddl\t$0x20, %esp");
			oc.println("\tmovl\t%ebp, %esp");
			oc.println("\tpopl\t%ebp");
			oc.println("\tret");
		case (Tail(), exp@(SET(_) | SETL (_) | Mov(_) | Neg(_) | Add(_, _) | Sub(_, _) | SLL(_, _) | Ld(_, _))) =>
			gdash(oc, (NonTail(regs(0)), exp));
			oc.println("\taddl\t$0x20, %esp");
			oc.println("\tmovl\t%ebp, %esp");
			oc.println("\tpopl\t%ebp");
			oc.println("\tret");
		case (Tail(), exp@(FMovD(_) | FNegD(_) | FAddD(_, _) | FSubD(_, _) | FMulD(_, _) | FDivD(_, _) | LdDF(_, _))) =>
			gdash(oc, (NonTail(fregs(0)), exp));
			oc.println("\taddl\t$0x20, %esp");
			oc.println("\tmovl\t%ebp, %esp");
			oc.println("\tpopl\t%ebp");
			oc.println("\tret");
		case (Tail(), exp@Restore(x)) =>
			locate(x) match {
				case List(i) => gdash(oc, (NonTail(regs(0)), exp))
				case List(i, j) if (i + 1 == j) => gdash(oc, (NonTail(fregs(0)), exp))
				case _ => throw new Exception()
			}
			oc.println("\taddl\t$0x20, %esp");
			oc.println("\tmovl\t%ebp, %esp");
			oc.println("\tpopl\t%ebp");
			oc.println("\tret");
		case (Tail(), IfEq(x, ydash, e1, e2)) =>
			oc.println("\tcmpl\t"+pp_id_or_imm(ydash)+", "+x);
			gdash_tail_if(oc, e1, e2, "je", "jne")
		case (Tail(), IfLE(x, ydash, e1, e2)) =>
			oc.println("\tcmpl\t"+pp_id_or_imm(ydash)+", "+x);
			gdash_tail_if(oc, e1, e2, "jle", "jg")
		case (Tail(), IfGE(x, ydash, e1, e2)) =>
			oc.println("\tcmpl\t"+pp_id_or_imm(ydash)+", "+x);
			gdash_tail_if(oc, e1, e2, "jge", "jl")
		case (Tail(), IfFEq(x, y, e1, e2)) =>
			oc.println("\tfcmpd\t"+x+", "+y);
			gdash_tail_if(oc, e1, e2, "fbe", "fbne")
		case (Tail(), IfFLE(x, y, e1, e2)) =>
			oc.println("\tfcmpd\t"+x+", "+y);
			gdash_tail_if(oc, e1, e2, "fble", "fbg");
		case (NonTail(z), IfEq(x, ydash, e1, e2)) =>
			oc.println("\tcmpl\t"+pp_id_or_imm(ydash)+", "+x);
			gdash_non_tail_if(oc, NonTail(z), e1, e2, "je", "jne")
		case (NonTail(z), IfLE(x, ydash, e1, e2)) =>
			oc.println("\tcmpl\t"+pp_id_or_imm(ydash)+", "+x);
			gdash_non_tail_if(oc, NonTail(z), e1, e2, "jle", "jg")
		case (NonTail(z), IfGE(x, ydash, e1, e2)) =>
			oc.println("\tcmpl\t"+pp_id_or_imm(ydash)+", "+x);
			gdash_non_tail_if(oc, NonTail(z), e1, e2, "jge", "jl")
		case (NonTail(z), IfFEq(x, y, e1, e2)) =>
			oc.println("\tfcmpd\t"+x+", "+y);
			gdash_non_tail_if(oc, NonTail(z), e1, e2, "fbe", "fbne")
		case (NonTail(z), IfFLE(x, y, e1, e2)) =>
			oc.println("\tfcmpd\t"+x+", "+y);
			gdash_non_tail_if(oc, NonTail(z), e1, e2, "fble", "fbg")
		// 関数呼び出しの仮想命令の実装 (caml2html: emit_call)
		case (Tail(), CallCls(x, ys, zs)) => // 末尾呼び出し (caml2html: emit_tailcall)
			gdash_args(oc, List((x, reg_cl)), ys, zs);
			oc.println("\tmovl\t0("+reg_cl+"), "+reg_sw);
			// adhoc, skip 6bytes to ignore stack push
			oc.println("\taddl\t$6, "+reg_sw);
			oc.println("\tjmp\t*"+reg_sw);
		case (Tail(), CallDir(x, ys, zs)) => // 末尾呼び出し
			gdash_args(oc, List(), ys, zs);
			oc.println("\tjmp\t"+x+"_tail");
		case (NonTail(a), CallCls(x, ys, zs)) =>
			gdash_args(oc, List((x, reg_cl)), ys, zs);
			val ss = stacksize ();
			oc.println("\tmovl\t"+reg_ra+", "+(ss - 4)+"("+reg_sp+")");
			oc.println("\tmovl\t("+reg_cl+"), "+reg_sw);
			oc.println("\tcall\t*"+reg_sw);

			if (allfregs.contains(a) && a != regs(0)) {
				oc.println("\tmovl\t"+regs(0)+", "+a)
			}
	/*
			oc.println("\tmovl\t"+reg_ra+", "+(ss - 4)+"("+reg_sp+")");
			oc.println("\tmovl\t("+reg_cl+"), "+reg_sw);
			oc.println("\tcall\t*"+reg_sw);
			oc.println("\tadd\t"+reg_sp+", "+ss+", "+reg_sp+"\t");
			oc.println("\tsubl\t"+ ss +", "+reg_sp);
			oc.println("\tmovl\t"+(ss - 4)+"("+reg_sp+"), "+reg_ra);
			if (allfregs.contains(a) && a != regs(0)) {
				oc.println("\tmov\t"+regs(0)+", "+a)
			} else if (allfregs.contains(a) && a != fregs(0)) {
				oc.println("\tfmovs\t"+fregs(0)+", "+a);
				oc.println("\tfmovs\t"+co_freg(fregs(0))+", "+co_freg(a))
			}
	*/
		case (NonTail(a), CallDir(x, ys, zs)) =>
			gdash_args(oc, List(), ys, zs);
			val ss = stacksize ();
			oc.println("\tmovl\t"+reg_ra+", "+(ss - 4)+"("+reg_sp+")");
			oc.println("\tcall\t"+x);
	/*
			oc.println("\tmovl\t"+(ss - 4)+"("+reg_sp+"), "+reg_ra);
	*/
			if (allfregs.contains(a) && a != regs(0)) {
				oc.println("\tmovl\t"+regs(0)+", "+a)
			}
	/*
			else if (allfregs.contains(a) && a != fregs(0)) {
				throw new Exception();
				oc.println("\tfmovs\t"+fregs(0)+", "+a);
				oc.println("\tfmovs\t"+co_freg(fregs(0))+", "+co_freg(a))
			}
	*/
	/*      oc.println("\tadd\t"+reg_sp+", "+ss+", "+reg_sp+"\t! delay slot"); */
	/*      oc.println("\tsub\t"+reg_sp+", "+ss+", "+reg_sp); */
	/*
			oc.println("\tld\t["+reg_sp+" + "+(ss - 4)+"], "+reg_ra);
			if (allfregs.contains(a) && a != regs(0)) {
				oc.println("\tmov\t"+regs(0)+", "+a)
			} else if (allfregs.contains(a) && a != fregs(0)) {
				oc.println("\tfmovs\t"+fregs(0)+", "+a);
				oc.println("\tfmovs\t"+co_freg(fregs(0))+", "+co_freg(a))
			}
	*/
	}

	def gdash_tail_if(oc:out_channel, e1:T, e2:T, b:String, bn:String) {
		val b_else = Id.genid(b + "_else");

		oc.println("\t"+bn+"\t"+b_else);

		val stackset_back = stackset;
		g(oc, (Tail(), e1));
		oc.println(""+b_else+":");
		stackset = stackset_back;
		g(oc, (Tail(), e2))
	}

	def gdash_non_tail_if(oc:out_channel, dest:Dest, e1:T, e2:T, b:String, bn:String) {
		val b_else = Id.genid(b + "_else");
		val b_cont = Id.genid(b + "_cont");
		
		oc.println("\t"+bn+"\t"+b_else);
		val stackset_back = stackset;
		g(oc, (dest, e1));
		val stackset1 = stackset;
		oc.println("\tjmp\t"+b_cont);
		oc.println(""+b_else+":");
		stackset = stackset_back;
		g(oc, (dest, e2));
		oc.println(""+b_cont+":");
		val stackset2 = stackset;
		stackset = stackset1 ** stackset2;
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

	def h(oc:out_channel, f:Fundef):Unit = f match {
		case Fundef(x, _, _, e, _) =>
			oc.println(""+x+":");
			oc.println("\tpushl\t%ebp");
			oc.println("\tmovl\t%esp,%ebp");
			oc.println("\tsubl\t$0x20,%esp");
			oc.println(""+x+"_tail:");
			stackset = Set[Selt]();
			stackmap = List[Selt]();
			g(oc, (Tail(), e))
	}

	def f(oc:out_channel, p:X86Asm.Prog):Unit = p.asInstanceOf[Prog] match {
		case Prog(data, fundefs, e) =>
			println("generating assembly...@.");
			oc.println(".section\t\".rodata\"");
			oc.println(".align\t8");
			data.foreach {
				case (x, d) =>
					oc.println(""+x+":\t! "+d);
					oc.println("\t.long\t0x"+gethi(d));
					oc.println("\t.long\t0x"+getlo(d))
			}
			oc.println(".section\t\".text\"");
			fundefs.foreach {
				case fundef => h(oc, fundef)
			}
			oc.println(".global\tmin_caml_start");
			oc.println("min_caml_start:");
			oc.println("\tpushl\t%ebp");
			oc.println("\tmovl\t%esp, %ebp");
			oc.println("\tsubl\t$0x20, %esp");
			oc.println("\tpushl\t%ebx");
			oc.println("\tpushl\t%esi");
			oc.println("\tpushl\t%edi");
			oc.println("\tmovl\t8(%esp),"+reg_hp)
			stackset = Set[Selt]();
			stackmap = List[Selt]();
			g(oc, (NonTail("%g0"), e));
			oc.println("\tpopl\t%edi");
			oc.println("\tpopl\t%esi");
			oc.println("\tpopl\t%ebx");
			oc.println("\taddl\t$0x20, %esp");
			oc.println("\tmovl\t%ebp, %esp");
			oc.println("\tpopl\t%ebp");
			oc.println("\tret");
	}
}
