package mincaml;
import java.io._;
import scala.collection.immutable._;


object Emit_x86 extends X86Asm {

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

	def save(x:Selt) {
		stackset = stackset + x;
		if (!(stackmap.contains(x))) {
			stackmap = stackmap ::: List(x)
		}
	}

	def savef(x:Selt) {
		stackset = stackset + x;
		if (!stackmap.contains(x)) {
			val pad:List[Selt] =
				if(stackmap.size % 2 == 0) {
					List[Selt]()
				} else {
					List[Selt](Id.gentmp(Type.Int()))
				}
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
	def offset(x:Selt):Int = {
		locate(x) match {
			case x::xs => 4 * x
			case _ => 0
		}
	}
	def stacksize():Int = {
		align((stackmap.size + 1) * 4)
	}

	def pp_id_or_imm(e:id_or_imm):Id.T = {
		e match {
			case V(x) => x
			case C(i) => "$" + i
		}
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


	// val g : Asm -> dest * X86Asm.t -> unit = <fun>
  
	// 命令列のアセンブリ生成 (caml2html: emit_g)
	def g(asm:Asm, e:(Dest, T)) {
		e match { 
		case (dest, Ans(exp))            => gdash(asm, (dest, exp))
		case (dest, Let((x, t), exp, e)) => gdash(asm, (NonTail(x), exp)); g(asm, (dest, e))
		case (_, Forget(_, _))           => throw new Exception()
		}
	}
	// val gdash : Asm -> dest * X86Asm.exp -> unit = <fun>

	// 各命令のアセンブリ生成 (caml2html: emit_gprime)
	def gdash(asm:Asm, e:(Dest,Exp)) {
		e match {

		// 末尾でなかったら計算結果をdestにセット (caml2html: emit_nontail)
		case (NonTail(_), Nop()) =>
		case (NonTail(x), SET(i)) => asm.movl("$" + i, x)
		case (NonTail(x), SETL(y)) => asm.movl("$" + y, x)
		case (NonTail(x), Mov(y)) if (x == y) =>
		case (NonTail(x), Mov(y)) => asm.movl(y, x)
		case (NonTail(x), Neg(y)) =>
			if (x != y) {
				asm.movl(y, x)
			}
			asm.negl(x)
		case (NonTail(x), Add(y, zdash)) =>
			if (x == y) {
				asm.addl(pp_id_or_imm(zdash), x)
			} else if (x == pp_id_or_imm(zdash)) {
				asm.addl(y, x)
			} else {
				asm.movl(y, x)
				asm.addl(pp_id_or_imm(zdash), x)
			}
		case (NonTail(x), Sub(y, zdash)) =>
			if (x == y) {
				asm.subl(pp_id_or_imm(zdash), x)
			} else if (x == pp_id_or_imm(zdash)) {
				asm.subl(y, x)
				asm.negl(x)
			} else {
				asm.movl(y, x)
				asm.subl(pp_id_or_imm(zdash), x)
			}
		case (NonTail(x), SLL(y, zdash)) => asm.sll(y, pp_id_or_imm(zdash), x)
		case (NonTail(x), Ld(y, zdash)) =>
			zdash match {
				case V(x) => throw new Exception();
				case C(i) => asm.movl(i+"("+y+")", x)
			}
		case (NonTail(_), St(x, y, zdash)) =>
			zdash match {
				case V(x) => throw new Exception();
				case C(i) =>  asm.movl(x, i+"("+y+")")
			}
		case (NonTail(x), FMovD(y)) if (x == y) =>
		case (NonTail(x), FMovD(y)) =>
			asm.fmovs(y, x);
			asm.fmovs(co_freg(y), co_freg(x));
		case (NonTail(x), FNegD(y)) =>
			asm.fnegs(y, x);
			if (x != y) asm.fmovs(co_freg(y), co_freg(x));
		case (NonTail(x), FAddD(y, z)) => asm.faddd(y, z, x)
		case (NonTail(x), FSubD(y, z)) => asm.fsubd(y, z, x)
		case (NonTail(x), FMulD(y, z)) => asm.fmuld(y, z, x)
		case (NonTail(x), FDivD(y, z)) => asm.fdivd(y, z, x)
		case (NonTail(x), LdDF(y, zdash)) => asm.ldd("["+y+" + "+pp_id_or_imm(zdash)+"]", x)
		case (NonTail(_), StDF(x, y, zdash)) => asm.std(x, "["+y+" + "+pp_id_or_imm(zdash)+"]")
		case (NonTail(_), Comment(s)) => asm.comment(s) 
		// 退避の仮想命令の実装 (caml2html: emit_save)
		case (NonTail(_), Save(x, y)) if (allregs.contains(x) && !stackset.contains(y)) =>
			save(y);
			asm.movl(x, offset(y)+"("+reg_sp+")")
		case (NonTail(_), Save(x, y)) if (allfregs.contains(x) && !stackset.contains(y)) =>
			savef(y);
			asm.std(x, "["+reg_sp+" + "+offset(y)+"]");
		case (NonTail(_), Save(x, y)) => if(!stackset.contains(y)) throw new Exception();
		// 復帰の仮想命令の実装 (caml2html: emit_restore)
		case (NonTail(x), Restore(y)) if (allregs.contains(x)) =>
			asm.movl(offset(y)+"("+reg_sp+")", x)
		case (NonTail(x), Restore(y)) =>
			if(!allfregs.contains(x))throw new Exception();
			asm.ldd("["+reg_sp+" + "+offset(y)+"]", x)
		// 末尾だったら計算結果を第一レジスタにセットしてret (caml2html: emit_tailret)
		case (Tail(), exp@(Nop() | St(_, _, _) | StDF(_, _, _) | Comment(_) | Save(_, _))) =>
			gdash(asm, (NonTail(Id.gentmp(Type.Unit())), exp));
			asm.addl("$0x20", "%esp");
			asm.movl("%ebp", "%esp");
			asm.popl("%ebp");
			asm.ret();
		case (Tail(), exp@(SET(_) | SETL (_) | Mov(_) | Neg(_) | Add(_, _) | Sub(_, _) | SLL(_, _) | Ld(_, _))) =>
			gdash(asm, (NonTail(regs(0)), exp));
			asm.addl("$0x20", "%esp");
			asm.movl("%ebp", "%esp");
			asm.popl("%ebp");
			asm.ret();
		case (Tail(), exp@(FMovD(_) | FNegD(_) | FAddD(_, _) | FSubD(_, _) | FMulD(_, _) | FDivD(_, _) | LdDF(_, _))) =>
			gdash(asm, (NonTail(fregs(0)), exp));
			asm.addl("$0x20", "%esp");
			asm.movl("%ebp", "%esp");
			asm.popl("%ebp");
			asm.ret();
		case (Tail(), exp@Restore(x)) =>
			locate(x) match {
				case List(i) => gdash(asm, (NonTail(regs(0)), exp))
				case List(i, j) if (i + 1 == j) => gdash(asm, (NonTail(fregs(0)), exp))
				case _ => throw new Exception()
			}
			asm.addl("$0x20", "%esp");
			asm.movl("%ebp", "%esp");
			asm.popl("%ebp");
			asm.ret();
		case (Tail(), IfEq(x, ydash, e1, e2)) =>
			asm.cmpl(pp_id_or_imm(ydash), x);
			gdash_tail_if(asm, e1, e2, "je", asm.jne(_))
		case (Tail(), IfLE(x, ydash, e1, e2)) =>
			asm.cmpl(pp_id_or_imm(ydash), x);
			gdash_tail_if(asm, e1, e2, "jle", asm.jg(_))
		case (Tail(), IfGE(x, ydash, e1, e2)) =>
			asm.cmpl(pp_id_or_imm(ydash), x);
			gdash_tail_if(asm, e1, e2, "jge", asm.jl(_))
		case (Tail(), IfFEq(x, y, e1, e2)) =>
			asm.fcmpd(x, y);
			gdash_tail_if(asm, e1, e2, "fbe", asm.fbne(_))
		case (Tail(), IfFLE(x, y, e1, e2)) =>
			asm.fcmpd(x, y);
			gdash_tail_if(asm, e1, e2, "fble", asm.fbg(_))
		case (NonTail(z), IfEq(x, ydash, e1, e2)) =>
			asm.cmpl(pp_id_or_imm(ydash), x);
			gdash_non_tail_if(asm, NonTail(z), e1, e2, "je", asm.jne(_))
		case (NonTail(z), IfLE(x, ydash, e1, e2)) =>
			asm.cmpl(pp_id_or_imm(ydash), x);
			gdash_non_tail_if(asm, NonTail(z), e1, e2, "jle", asm.jg(_))
		case (NonTail(z), IfGE(x, ydash, e1, e2)) =>
			asm.cmpl(pp_id_or_imm(ydash), x);
			gdash_non_tail_if(asm, NonTail(z), e1, e2, "jge", asm.jl(_))
		case (NonTail(z), IfFEq(x, y, e1, e2)) =>
			asm.fcmpd(x, y);
			gdash_non_tail_if(asm, NonTail(z), e1, e2, "fbe", asm.fbne(_))
		case (NonTail(z), IfFLE(x, y, e1, e2)) =>
			asm.fcmpd(x, y);
			gdash_non_tail_if(asm, NonTail(z), e1, e2, "fble", asm.fbg(_))
		// 関数呼び出しの仮想命令の実装 (caml2html: emit_call)
		case (Tail(), CallCls(x, ys, zs)) => // 末尾呼び出し (caml2html: emit_tailcall)
			gdash_args(asm, List((x, reg_cl)), ys, zs);
			asm.movl("0("+reg_cl+")", reg_sw);
			// adhoc, skip 6bytes to ignore stack push
			asm.addl("$6", reg_sw);
			asm.jmp("*" + reg_sw);
		case (Tail(), CallDir(x, ys, zs)) => // 末尾呼び出し
			gdash_args(asm, List(), ys, zs);
			asm.jmp(x + "_tail");
		case (NonTail(a), CallCls(x, ys, zs)) =>
			gdash_args(asm, List((x, reg_cl)), ys, zs);
			val ss = stacksize ();
			asm.movl(reg_ra, (ss - 4) + "(" + reg_sp + ")");
			asm.movl("(" + reg_cl + ")", reg_sw);
			asm.call("*"+reg_sw);

			if (allfregs.contains(a) && a != regs(0)) {
				asm.movl(regs(0), a)
			}
	/*
			asm.movl(reg_ra, (ss - 4)+"("+reg_sp+")");
			asm.movl("("+reg_cl+")", reg_sw);
			asm.call("*"+reg_sw);
			asm.add(reg_sp, ss, reg_sp);
			asm.subl(ss, reg_sp);
			asm.movl((ss - 4)+"("+reg_sp+")", reg_ra);
			if (allfregs.contains(a) && a != regs(0)) {
				asm.mov(regs(0), a)
			} else if (allfregs.contains(a) && a != fregs(0)) {
				asm.fmovs(fregs(0), a);
				asm.fmovs(co_freg(fregs(0)), co_freg(a))
			}
	*/
		case (NonTail(a), CallDir(x, ys, zs)) =>
			gdash_args(asm, List(), ys, zs);
			val ss = stacksize ();
			asm.movl(reg_ra, (ss - 4) + "(" + reg_sp + ")");
			asm.call(x);
	/*
			asm.movl((ss - 4)+"("+reg_sp+")", reg_ra);
	*/
			if (allfregs.contains(a) && a != regs(0)) {
				asm.movl(regs(0), a)
			}
	/*
			else if (allfregs.contains(a) && a != fregs(0)) {
				throw new Exception();
				asm.fmovs(fregs(0), a);
				asm.fmovs(co_freg(fregs(0)), co_freg(a))
			}
	*/
	/*      asm.add(reg_sp, ss, reg_sp+"\t! delay slot"); */
	/*      asm.sub(reg_sp, ss, reg_sp); */
	/*
			asm.ld("["+reg_sp+" + "+(ss - 4)+"]", reg_ra);
			if (allfregs.contains(a) && a != regs(0)) {
				asm.mov(regs(0), a)
			} else if (allfregs.contains(a) && a != fregs(0)) {
				asm.fmovs(fregs(0), a);
				asm.fmovs(co_freg(fregs(0)), co_freg(a))
			}
	*/
		}
	}

	def gdash_tail_if(asm:Asm, e1:T, e2:T, b:String, bn:(String)=>Unit) {
		val b_else = Id.genid(b + "_else");

		bn(b_else);

		val stackset_back = stackset;
		g(asm, (Tail(), e1));
		asm.label(b_else);
		stackset = stackset_back;
		g(asm, (Tail(), e2))
	}

	def gdash_non_tail_if(asm:Asm, dest:Dest, e1:T, e2:T, b:String, bn:(String)=>Unit) {
		val b_else = Id.genid(b + "_else");
		val b_cont = Id.genid(b + "_cont");
		
		bn(b_else);
		val stackset_back = stackset;
		g(asm, (dest, e1));
		val stackset1 = stackset;
		asm.jmp(b_cont);
		asm.label(b_else);
		stackset = stackset_back;
		g(asm, (dest, e2));
		asm.label(b_cont);
		val stackset2 = stackset;
		stackset = stackset1 ** stackset2;
	}

	// val g'_args :
	//  Asm -> (Id.t * Id.t) list -> Id.t list -> Id.t list -> unit = <fun>
	def gdash_args(asm:Asm, x_reg_cl:List[(Id.T, Id.T)], ys:List[Id.T], zs:List[Id.T]) {

		val (i, yrs) = ys.foldLeft((0, x_reg_cl)) {
			case ((i, yrs), y) => (i + 1, (y, regs(i)) :: yrs)
		}

		shuffle(reg_sw, yrs).foreach {
			case (y, r) => asm.mov(y, r)
		}

		val (d, zfrs) = zs.foldLeft((0, List[(Id.T, String)]())){
			case ((d, zfrs), z) => (d + 1, (z, fregs(d)) :: zfrs)
		}

		shuffle(reg_fsw, zfrs).foreach {
			case (z, fr) =>
				asm.fmovs(z, fr);
				asm.fmovs(co_freg(z), co_freg(fr));
		}
	}

	def h(asm:Asm, f:Fundef) {
		f match {
		case Fundef(x, _, _, e, _) =>
			asm.label(x);
			asm.pushl("%ebp");
			asm.movl("%esp", "%ebp");
			asm.subl("$0x20", "%esp");
			asm.label(x+"_tail");
			stackset = Set[Selt]();
			stackmap = List[Selt]();
			g(asm, (Tail(), e))
		}
	}

	def apply(o:PrintWriter, p:X86Asm.Prog) {
		val asm = new Asm(o)
		p.asInstanceOf[Prog] match {
		case Prog(data, fundefs, e) =>
			println("generating assembly...@.");
			asm._section(".rodata");
			asm._align(8);
			data.foreach {
				case (x, d) =>
					asm.label(x);
					asm.comment("" + d);
					asm._long(gethi(d));
					asm._long(getlo(d))
			}
			asm._section(".text");
			fundefs.foreach {
				case fundef => h(asm, fundef)
			}
			asm._global("_min_caml_start");
			asm.label("_min_caml_start");
			asm.pushl("%ebp");
			asm.movl("%esp", "%ebp");
			asm.subl("$0x20", "%esp");
			asm.pushl("%ebx");
			asm.pushl("%esi");
			asm.pushl("%edi");
			asm.movl("8(%esp)", reg_hp)
			stackset = Set[Selt]();
			stackmap = List[Selt]();
			g(asm, (NonTail("%g0"), e));
			asm.popl("%edi");
			asm.popl("%esi");
			asm.popl("%ebx");
			asm.addl("$0x20", "%esp");
			asm.movl("%ebp", "%esp");
			asm.popl("%ebp");
			asm.ret();
		}
	}
}

class Asm(p:PrintWriter) {
	def _section(r1:String) {
		p.println("\t.section\t" + r1)
	}
	def _global(r1:String) {
		p.println("\t.global\t" + r1)
	}
	def _align(r1:Int) {
		p.println("\t.align\t" + r1)
	}
	def _long(s:Int) {
		p.println("\t.long\t" + s);
	}
	def _globl(r1:String) {
		p.println("\t.globl\t"+ r1)
	}
	def comment(d:String) {
		p.println("! "+ d)
	}
	def label(r1:String) {
		p.println(r1 + ":")
	}
	def pushl(r1:String) {
		p.println("\tpushl\t" + r1)
	}
	def popl(r1:String) {
		p.println("\tpopl\t" + r1);
	}
	def mov(r1:String, r2:String) {
		p.println("\tmov\t" + r1 + "," + r2)
	}
	def movl(r1:String, r2:String) {
		p.println("\tmovl\t" + r1 + "," + r2)
	}
	def addl(r1:String, r2:String) {
		p.println("\taddl\t" + r1 + "," + r2)
	}
	def subl(r1:String, r2:String) {
		p.println("\tsubl\t" + r1 + "," + r2)
	}
	def cmpl(r1:String, r2:String) {
		p.println("\tcmpl\t" + r1 + "," + r2)
	}
	def negl(r1:String) {
		p.println("\tnegl\t" + r1)
	}
	def ldd(r1:String, r2:String) {
		p.println("\tldd\t" + r1 + "," + r2)
	}
	def std(r1:String, r2:String) {
		p.println("\tstd\t" + r1 + "," + r2)
	}
	def fmovs(r1:String, r2:String) {
		p.println("\tfmovs\t" + r1 + "," + r2)
	}
	def fnegs(r1:String, r2:String) {
		p.println("\tfnegs\t" + r1 + "," + r2)
	}
	def faddd(r1:String, r2:String, r3:String) {
		p.println("\tfaddd\t" + r1 + "," + r2 + "," + r3)
	}
	def fsubd(r1:String, r2:String, r3:String) {
		p.println("\tfsubd\t" + r1 + "," + r2 + "," + r3)
	}
	def fmuld(r1:String, r2:String, r3:String) {
		p.println("\tfmuld\t" + r1 + "," + r2 + "," + r3)
	}
	def sll(r1:String, r2:String, r3:String) {
		p.println("\tsll\t" + r1 + "," + r2 + "," + r3)
	}
	def fcmpd(r1:String, r2:String) {
		p.println("\tfcmpd\t" + r1 + "," + r2)
	}
	def fdivd(r1:String, r2:String, r3:String) {
		p.println("\tfdivd\t" + r1 + "," + r2 + "," + r3)
	}
	def jmp(r1:String) {
		p.println("\tjmp\t" + r1)
	}
	def call(opds:String) {
		p.println("\tcall\t" + opds)
	}
	def ret() {
		p.println("\tret")
	}
	def add(r1:String, r2:String, r3:String) {
		p.println("\tadd " + r1 + "," + r2 + "," + r3)
	}
	def jne(r1:String) {
		p.println("\tjne\t" + r1)
	}
	def jg(r1:String) {
		p.println("\tjg\t" + r1)
	}
	def jl(r1:String) {
		p.println("\tjl\t" + r1)
	}
	def fbne(r1:String) {
		p.println("\tfbne\t" + r1)
	}
	def fbg(r1:String) {
		p.println("\tfbg\t" + r1)
	}
}
