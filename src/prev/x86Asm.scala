// x86 assembly with a few virtual instructions

abstruct sealed class id_or_imm()
case class V(Id.t) extends id_or_imm
case class C(int) extends id_or_imm

sealed abstract class T // 命令の列 (caml2html: sparcasm_t)
case class Ans(a:exp) extends T
case class Let(a:(Id.t, Type.t), b:Exp, c:T) extends T
case class Forget(a:Id.t, b:T) extends T // Spillされた変数を、自由変数の計算から除外するための仮想命令 (caml2html: sparcasm_forget)

sealed abstract class Exp // 一つ一つの命令に対応する式 (caml2html: sparcasm_exp)
case class Nop() extends Exp
case class Set(a:int) extends Exp
case class SetL(a:Id.l) extends Exp
case class Mov(a:Id.t) extends Exp
case class Neg(a:Id.t) extends Exp
case class Add(a:Id.t, b:id_or_imm) extends Exp
case class Sub(a:Id.t, b:id_or_imm) extends Exp
case class SLL(a:Id.t, b:id_or_imm) extends Exp
case class Ld(a:Id.t, b:id_or_imm) extends Exp
case class St(a:Id.t, b:Id.t, c:id_or_imm) extends Exp
case class FMovD(a:Id.t) extends Exp
case class FNegD(a:Id.t) extends Exp
case class FAddD(a:Id.t, b:Id.t) extends Exp
case class FSubD(a:Id.t, b:Id.t) extends Exp
case class FMulD(a:Id.t, b:Id.t) extends Exp
case class FDivD(a:Id.t, b:Id.t) extends Exp
case class LdDF(a:Id.t, b:id_or_imm) extends Exp
case class StDF(a:Id.t, b:Id.t, c:id_or_imm) extends Exp
case class Comment(a:string) extends Exp
  // virtual instructions
case class IfEq(a:Id.t, b:id_or_imm, c:t, d:t) extends Exp
case class IfLE(a:Id.t, b:id_or_imm, c:t, d:t) extends Exp
case class IfGE(a:Id.t, b:id_or_imm, c:t, d:t) extends Exp // 左右対称ではないので必要
case class IfFEq(a:Id.t, b:Id.t, c:t, d:t) extends Exp
case class IfFLE(a:Id.t, b:Id.t, c:t, d:t) extends Exp
  // closure address, integer arguments, and float arguments
case class CallCls(a:Id.t, b:List[Id.t], c:List[Id.t]) extends Exp
case class CallDir(a:Id.l, b:List[Id.t], c:List[Id.t]) extends Exp
case class Save(a:Id.t, b:Id.t) extends Exp // レジスタ変数の値をスタック変数へ保存 (caml2html: sparcasm_save)
case class Restore(a:Id.t) extends Exp // スタック変数から値を復元 (caml2html: sparcasm_restore)

case class Fundef(name:Id.l, args:List[Id.t], fargs:List[Id.t], body:T, ret:Type.t)

// プログラム全体 = 浮動小数定数テーブル + トップレベル関数 + メインの式 (caml2html: sparcasm_prog)
case class Prog(a:List[(Id.l, float)],b:List[Fundef],c:T)

def fletd(x, e1, e2) = Let((x, Type.Float), e1, e2)
def seq(e1, e2) = Let((Id.gentmp(Type.Unit), Type.Unit), e1, e2)

let regs = // Array.init 16 (fun i => Printf.sprintf "%%r%d" i)
  Array( "%eax", "%ebx", "%ecx", "%edx", "%esi", /*, "%edi" */ )

val fregs = Array.init(16, i => Printf.sprintf("%%f%d", (i * 2)))
val allregs = Array.to_list(regs)
val allfregs = Array.to_list(fregs)
val reg_cl = regs(Array.length, regs - 1) // closure address (caml2html: sparcasm_regcl)
val reg_sw = regs(Array.length, regs - 2) // temporary for swap
val reg_fsw = fregs(Array.length, fregs - 1) // temporary for swap
val reg_sp = "%esp" // stack pointer
val reg_hp = "%edi" // heap pointer (caml2html: sparcasm_reghp)
val reg_ra = "%eax" // return address
def is_reg(x:String):Boolean = {
	x(0) == '%'
}
def co_freg_table = {
  val ht = Hashtbl.create(16);
  for(i <- 0 to 15) {
    Hashtbl.add(ht, "%f" + (i * 2), "%f" + (i * 2 + 1) )
  }
  ht
}

def co_freg(freg) = Hashtbl.find(co_freg_table, freg) // "companion" freg

// super-tenuki
def remove_and_uniq (xs, e) = e match {
	case List()                    => List()
	case x :: ys if (S.mem(x, xs)) => remove_and_uniq(xs, ys)
	case x :: ys                   => x :: remove_and_uniq(S.add(x, xs), ys)
}

// free variables in the order of use (for spilling) (caml2html: sparcasm_fv) 
def fv_id_or_imm(a) = a match {
	case V(x) => List(x)
	case _    => List()
}

def fv_exp(cont, e) = e match {
	case Nop | Set(_) | SetL(_) | Comment(_) | Restore(_) => cont
	case Mov(x) => x :: cont
	case Neg(x) => x :: cont
	case FMovD(x) => x :: cont
	case FNegD(x) => x :: cont
	case Save(x, _) => x :: cont
	case Add(x, y)  => x :: fv_id_or_imm(y ::: cont)
	case Sub(x, y)  => x :: fv_id_or_imm(y ::: cont)
	case SLL(x, y)  => x :: fv_id_or_imm(y ::: cont)
	case Ld(x, y)   => x :: fv_id_or_imm(y ::: cont)
	case LdDF(x, y) => x :: fv_id_or_imm(y ::: cont)
	case St(x, y, z)   => x :: y :: fv_id_or_imm (z ::: cont)
	case StDF(x, y, z) => x :: y :: fv_id_or_imm (z ::: cont)
	case FAddD(x, y) => x :: y :: cont
	case FSubD(x, y) => x :: y :: cont
	case FMulD(x, y) => x :: y :: cont
	case FDivD(x, y) => x :: y :: cont
	case IfEq(x, y, e1, e2) => x :: fv_id_or_imm(y) ::: remove_and_uniq(S.empty, fv(cont, e1) ::: fv(cont, e2)) // uniq here just for efficiency
	case IfLE(x, y, e1, e2) => x :: fv_id_or_imm(y) ::: remove_and_uniq(S.empty, fv(cont, e1) ::: fv(cont, e2)) // uniq here just for efficiency
	case IfGE(x, y, e1, e2) => x :: fv_id_or_imm(y) ::: remove_and_uniq(S.empty, fv(cont, e1) ::: fv(cont, e2)) // uniq here just for efficiency
	case IfFEq(x, y, e1, e2) => x :: y :: remove_and_uniq(S.empty(fv(cont, e1) ::: fv(cont, e2))) // uniq here just for efficiency
	case IfFLE(x, y, e1, e2) => x :: y :: remove_and_uniq(S.empty(fv(cont, e1) ::: fv(cont, e2))) // uniq here just for efficiency
	case CallCls(x, ys, zs) => x :: ys ::: zs ::: cont
	case CallDir(_, ys, zs) => ys ::: zs ::: cont
}

def fv(cont,e) = e match {
	case Ans(exp) => fv_exp(cont, exp)
	case Let((x, t), exp, e) =>
		val cont = remove_and_uniq(S.singleton(x), fv(cont, e));
		fv_exp(cont, exp)
	case Forget(x, e) => remove_and_uniq (S.singleton x) (fv cont e) // Spillされた変数は、自由変数の計算から除外 (caml2html: sparcasm_exclude)
	// (if y = z then (forget x; ...) else (forget x; ...)); x + x
	//   のような場合のために、継続の自由変数contを引数とする
}

def fv(e) = remove_and_uniq(S.empty(fv(List(), e));

def concat(e1, xt, e2) = e1 match {
	case Ans(exp) => Let(xt, exp, e2)
	case Let(yt, exp, e1) => Let(yt, exp, concat(e1, xt, e2))
	case Forget(y, e1) => Forget(y, concat(e1, xt, e2))
}
def align(i) = {
	if (i % 8 == 0) {
		i
	} else {
		i + 4
	}
}
