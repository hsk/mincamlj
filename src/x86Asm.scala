// x86 assembly with a few virtual instructions
package mincaml;

import scala.collection.immutable._;

class X86Asm {
	abstract sealed class id_or_imm()
	case class V(a:Id.T) extends id_or_imm
	case class C(a:Int) extends id_or_imm

	sealed abstract class T // 命令の列 (caml2html: sparcasm_t)
	case class Ans(a:Exp) extends T
	case class Let(a:(Id.T, Type.T), b:Exp, c:T) extends T
	case class Forget(a:Id.T, b:T) extends T // Spillされた変数を、自由変数の計算から除外するための仮想命令 (caml2html: sparcasm_forget)

	sealed abstract case class Exp // 一つ一つの命令に対応する式 (caml2html: sparcasm_exp)
	case class Nop() extends Exp
	case class SET(a:Int) extends Exp
	case class SETL(a:Id.L) extends Exp
	case class Mov(a:Id.T) extends Exp
	case class Neg(a:Id.T) extends Exp
	case class Add(a:Id.T, b:id_or_imm) extends Exp
	case class Sub(a:Id.T, b:id_or_imm) extends Exp
	case class SLL(a:Id.T, b:id_or_imm) extends Exp
	case class Ld(a:Id.T, b:id_or_imm) extends Exp
	case class St(a:Id.T, b:Id.T, c:id_or_imm) extends Exp
	case class FMovD(a:Id.T) extends Exp
	case class FNegD(a:Id.T) extends Exp
	case class FAddD(a:Id.T, b:Id.T) extends Exp
	case class FSubD(a:Id.T, b:Id.T) extends Exp
	case class FMulD(a:Id.T, b:Id.T) extends Exp
	case class FDivD(a:Id.T, b:Id.T) extends Exp
	case class LdDF(a:Id.T, b:id_or_imm) extends Exp
	case class StDF(a:Id.T, b:Id.T, c:id_or_imm) extends Exp
	case class Comment(a:String) extends Exp
	  // virtual instructions
	case class IfEq(a:Id.T, b:id_or_imm, c:T, d:T) extends Exp
	case class IfLE(a:Id.T, b:id_or_imm, c:T, d:T) extends Exp
	case class IfGE(a:Id.T, b:id_or_imm, c:T, d:T) extends Exp // 左右対称ではないので必要
	case class IfFEq(a:Id.T, b:Id.T, c:T, d:T) extends Exp
	case class IfFLE(a:Id.T, b:Id.T, c:T, d:T) extends Exp
	  // closure address, integer arguments, and float arguments
	case class CallCls(a:Id.T, b:List[Id.T], c:List[Id.T]) extends Exp
	case class CallDir(a:Id.L, b:List[Id.T], c:List[Id.T]) extends Exp
	case class Save(a:Id.T, b:Id.T) extends Exp // レジスタ変数の値をスタック変数へ保存 (caml2html: sparcasm_save)
	case class Restore(a:Id.T) extends Exp // スタック変数から値を復元 (caml2html: sparcasm_restore)

	case class Fundef(name:Id.L, args:List[Id.T], fargs:List[Id.T], body:T, ret:Type.T)

	// プログラム全体 = 浮動小数定数テーブル + トップレベル関数 + メインの式 (caml2html: sparcasm_prog)
	case class Prog(a:List[(Id.L, Double)],b:List[Fundef],c:T)
}

object X86Asm extends X86Asm {

// val fletd : Id.T * Exp * T -> T = <fun>

	def fletd(x:Id.T, e1:Exp, e2:T):T = Let((x, Type.Float()), e1, e2)
	def seq(e1:Exp, e2:T):T = Let((Id.gentmp(Type.Unit()), Type.Unit()), e1, e2)

	val regs = // Array.init 16 (fun i => Printf.sprintf "%%r%d" i)
	  Array( "%eax", "%ebx", "%ecx", "%edx", "%esi", /*, "%edi" */ )

	val fregs = Array("%f0", "%f2", "%f4", "%f6", "%f8", "%f10", "%f12", "%f14", "%f16",
    "%f18", "%f20", "%f22", "%f24", "%f26", "%f28", "%f30")
	val allregs = regs.toList
	val allfregs = fregs.toList
	val reg_cl = regs(regs.size - 1) // closure address (caml2html: sparcasm_regcl)
	val reg_sw = regs(regs.size - 2) // temporary for swap
	val reg_fsw = fregs(fregs.size - 1) // temporary for swap
	val reg_sp = "%esp" // stack pointer
	val reg_hp = "%edi" // heap pointer (caml2html: sparcasm_reghp)
	val reg_ra = "%eax" // return address
	def is_reg(x:String):Boolean = {
		x(0) == '%'
	}
	def co_freg_table:Map[String,String] = {
	  var ht = Map[String,String]()
	  for(i <- 0 to 15) {
	    ht = ht + (("%f" + (i * 2)) -> ("%f" + (i * 2 + 1)) )
	  }
	  ht
	}

	def co_freg(freg:String):String = co_freg_table(freg) // "companion" freg

	// super-tenuki
	def remove_and_uniq (xs:List[Id.T], e:List[Id.T]):List[Id.T] = e match {
		case List()                      => List()
		case x :: ys if (xs.contains(x)) => remove_and_uniq(xs, ys)
		case x :: ys                     => x :: remove_and_uniq(xs + x, ys)
	}

	// free variables in the order of use (for spilling) (caml2html: sparcasm_fv) 
	def fv_id_or_imm(a:id_or_imm):List[Id.T] = a match {
		case V(x) => List(x)
		case _    => List()
	}


	def fv_exp(cont:List[Id.T], e:Exp):List[Id.T] = {

		e match {
			case Nop() | SET(_) | SETL(_) | Comment(_) | Restore(_) => cont
			case Mov(x) => x :: cont
			case Neg(x) => x :: cont
			case FMovD(x) => x :: cont
			case FNegD(x) => x :: cont
			case Save(x, _) => x :: cont
			case Add(x:Id.T, y:id_or_imm)  => x :: fv_id_or_imm(y) ::: cont
			case Sub(x, y)  => x :: fv_id_or_imm(y) ::: cont
			case SLL(x, y)  => x :: fv_id_or_imm(y) ::: cont
			case Ld(x, y)   => x :: fv_id_or_imm(y) ::: cont
			case LdDF(x, y) => x :: fv_id_or_imm(y) ::: cont
			case St(x, y, z)   => x :: y :: fv_id_or_imm (z) ::: cont
			case StDF(x, y, z) => x :: y :: fv_id_or_imm (z) ::: cont
			case FAddD(x, y) => x :: y :: cont
			case FSubD(x, y) => x :: y :: cont
			case FMulD(x, y) => x :: y :: cont
			case FDivD(x, y) => x :: y :: cont
			case IfEq(x, y, e1, e2) => x :: fv_id_or_imm(y) ::: remove_and_uniq(List(), fv(cont, e1) ::: fv(cont, e2)) // uniq here just for efficiency
			case IfLE(x, y, e1, e2) => x :: fv_id_or_imm(y) ::: remove_and_uniq(List(), fv(cont, e1) ::: fv(cont, e2)) // uniq here just for efficiency
			case IfGE(x, y, e1, e2) => x :: fv_id_or_imm(y) ::: remove_and_uniq(List(), fv(cont, e1) ::: fv(cont, e2)) // uniq here just for efficiency
			case IfFEq(x, y, e1, e2) => x :: y :: remove_and_uniq(List(), (fv(cont, e1) ::: fv(cont, e2))) // uniq here just for efficiency
			case IfFLE(x, y, e1, e2) => x :: y :: remove_and_uniq(List(), (fv(cont, e1) ::: fv(cont, e2))) // uniq here just for efficiency
			case CallCls(x, ys, zs) => x :: ys ::: zs ::: cont
			case CallDir(_, ys, zs) => ys ::: zs ::: cont
		}
	}

	def fv(cont:List[Id.T], e:T):List[Id.T] = e match {
		case Ans(exp) => fv_exp(cont, exp)
		case Let((x, t), exp, e) =>
			val cont2 = remove_and_uniq(List(x), fv(cont, e));
			fv_exp(cont2, exp)
		case Forget(x, e) => remove_and_uniq(List(x), fv(cont, e)) // Spillされた変数は、自由変数の計算から除外 (caml2html: sparcasm_exclude)
		// (if y = z then (forget x; ...) else (forget x; ...)); x + x
		//   のような場合のために、継続の自由変数contを引数とする
	}

	def fv(e:T):List[Id.T] = remove_and_uniq(List(), fv(List(), e));

	def concat(e1:T, xt:(Id.T,Type.T), e2:T):T = e1 match {
		case Ans(exp) => Let(xt, exp, e2)
		case Let(yt, exp, e1) => Let(yt, exp, concat(e1, xt, e2))
		case Forget(y, e1) => Forget(y, concat(e1, xt, e2))
	}
	def align(i:Int):Int = {
		if (i % 8 == 0) {
			i
		} else {
			i + 4
		}
	}
}
