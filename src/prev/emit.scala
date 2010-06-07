open SparcAsm

external gethi : float => int32 = "gethi"
external getlo : float => int32 = "getlo"

var stackset = S.empty (* すでにSaveされた変数の集合 (caml2html: emit_stackset) *)
var stackmap = List() (* Saveされた変数の、スタックにおける位置 (caml2html: emit_stackmap) *)

def save(x) = {
  stackset = S.add(x, stackset);
  if (not(List.mem(x,stackmap))) {
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
						case (y', z) if (y == y') => (sw, z)
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
  case dest, Ans(exp) => g' oc (dest, exp)
  case dest, Let((x, t), exp, e) =>
      g' oc (NonTail(x), exp);
      g oc (dest, e)
  case _, Forget _ => assert false
}

// 各命令のアセンブリ生成 (caml2html: emit_gprime)
def g'(oc, e) = e match {
  // 末尾でなかったら計算結果をdestにセット (caml2html: emit_nontail)
  case (NonTail(_), Nop => ()
  case (NonTail(x), Set(i) => oc.print("\tset\t"+i+", "+x+"\n")
  case (NonTail(x), SetL(Id.L(y)) => oc.print("\tset\t"+y+", "+x+"\n")
  case (NonTail(x), Mov(y) when x = y => ()
  case (NonTail(x), Mov(y) => oc.print("\tmov\t"+y+", "+x+"\n")
  case (NonTail(x), Neg(y) => oc.print("\tneg\t"+y+", "+x+"\n")
  case (NonTail(x), Add(y, z') => oc.print("\tadd\t"+y+", "+pp_id_or_imm(z')+", "+x+"\n")
  case (NonTail(x), Sub(y, z') => oc.print("\tsub\t%s, %s, %s\n", y, (pp_id_or_imm z'), x)
  case (NonTail(x), SLL(y, z') => oc.print("\tsll\t%s, %s, %s\n", y, (pp_id_or_imm z'), x)
  case (NonTail(x), Ld(y, z') => oc.print("\tld\t[%s + %s], %s\n", y, (pp_id_or_imm z'), x)
  case (NonTail(_), St(x, y, z') => oc.print("\tst\t%s, [%s + %s]\n", x, y, pp_id_or_imm(z'))
  case (NonTail(x), FMovD(y) when x = y => ()
  case (NonTail(x), FMovD(y) =>
      oc.print("\tfmovs\t%s, %s\n", y, x);
      oc.print("\tfmovs\t%s, %s\n",co_freg(y), co_freg(x));
  case (NonTail(x), FNegD(y) =>
      oc.print("\tfnegs\t%s, %s\n", y, x);
      if x <> y then oc.print("\tfmovs\t%s, %s\n" (co_freg y) (co_freg x)
  case (NonTail(x), FAddD(y, z) => oc.print("\tfaddd\t%s, %s, %s\n" y z x)
  case (NonTail(x), FSubD(y, z) => oc.print("\tfsubd\t%s, %s, %s\n" y z x)
  case (NonTail(x), FMulD(y, z) => oc.print("\tfmuld\t%s, %s, %s\n" y z x)
  case (NonTail(x), FDivD(y, z) => oc.print("\tfdivd\t%s, %s, %s\n" y z x)
  case (NonTail(x), LdDF(y, z') => oc.print("\tldd\t[%s + %s], %s\n" y (pp_id_or_imm z') x)
  case (NonTail(_), StDF(x, y, z') => oc.print("\tstd\t%s, [%s + %s]\n" x y (pp_id_or_imm z'))
  case (NonTail(_), Comment(s) => oc.print("\t! %s\n" s)
  (* 退避の仮想命令の実装 (caml2html: emit_save) *)
  case (NonTail(_), Save(x, y) when List.mem x allregs && not (S.mem y !stackset) =>
      save y;
      oc.print("\tst\t%s, [%s + %d]\n" x reg_sp (offset y))
  case (NonTail(_), Save(x, y) when List.mem x allfregs && not (S.mem y !stackset) =>
      savef y;
      oc.print("\tstd\t%s, [%s + %d]\n" x reg_sp (offset y))
  case (NonTail(_), Save(x, y) => assert (S.mem y !stackset); ()
  (* 復帰の仮想命令の実装 (caml2html: emit_restore) *)
  case (NonTail(x), Restore(y) when List.mem x allregs =>
      oc.print("\tld\t[%s + %d], %s\n" reg_sp (offset y) x)
  case (NonTail(x), Restore(y) =>
      assert (List.mem x allfregs);
      oc.print("\tldd\t[%s + %d], %s\n" reg_sp (offset y) x)
  (* 末尾だったら計算結果を第一レジスタにセットしてret (caml2html: emit_tailret) *)
  case (Tail(), exp@(Nop() | St(_) | StDF(_) | Comment(_) | Save(_))) =>
      g' oc (NonTail(Id.gentmp Type.Unit), exp);
      oc.print("\tretl\n");
      oc.print("\tnop\n")
  case (Tail, exp@(Set(_) | SetL (_) | Mov(_) | Neg(_) | Add(_) | Sub(_) | SLL(_) | Ld(_))) =>
      g' oc (NonTail(regs.(0)), exp);
      oc.print("\tretl\n");
      oc.print("\tnop\n")
  case (Tail, exp@(FMovD(_) | FNegD(_) | FAddD(_) | FSubD(_) | FMulD(_) | FDivD(_) | LdDF(_)) =>
      g' oc (NonTail(fregs.(0)), exp);
      oc.print("\tretl\n");
      oc.print("\tnop\n")
  case (Tail, exp@Restore(x)) =>
      locate(x) match {
      | [i] => g' oc (NonTail(regs.(0)), exp)
      | [i; j] when i + 1 = j => g' oc (NonTail(fregs.(0)), exp)
      | _ => assert false
      }
      oc.print("\tretl\n");
      oc.print("\tnop\n")
  case (Tail, IfEq(x, y', e1, e2)) =>
      oc.print("\tcmp\t%s, %s\n" x (pp_id_or_imm y'));
      g'_tail_if oc e1 e2 "be" "bne"
  case (Tail, IfLE(x, y', e1, e2)) =>
      oc.print("\tcmp\t%s, %s\n" x (pp_id_or_imm y'));
      g'_tail_if oc e1 e2 "ble" "bg"
  case (Tail, IfGE(x, y', e1, e2)) =>
      oc.print("\tcmp\t%s, %s\n" x (pp_id_or_imm y'));
      g'_tail_if oc e1 e2 "bge" "bl"
  case (Tail, IfFEq(x, y, e1, e2)) =>
      oc.print("\tfcmpd\t%s, %s\n" x y);
      oc.print("\tnop\n");
      g'_tail_if oc e1 e2 "fbe" "fbne"
  case (Tail, IfFLE(x, y, e1, e2)) =>
      oc.print("\tfcmpd\t%s, %s\n" x y);
      oc.print("\tnop\n");
      g'_tail_if oc e1 e2 "fble" "fbg"
  case (NonTail(z), IfEq(x, y', e1, e2)) =>
      oc.print("\tcmp\t%s, %s\n" x (pp_id_or_imm y'));
      g'_non_tail_if oc (NonTail(z)) e1 e2 "be" "bne"
  case (NonTail(z), IfLE(x, y', e1, e2)) =>
      oc.print("\tcmp\t%s, %s\n" x (pp_id_or_imm y'));
      g'_non_tail_if oc (NonTail(z)) e1 e2 "ble" "bg"
  case (NonTail(z), IfGE(x, y', e1, e2)) =>
      oc.print("\tcmp\t%s, %s\n" x (pp_id_or_imm y'));
      g'_non_tail_if oc (NonTail(z)) e1 e2 "bge" "bl"
  case (NonTail(z), IfFEq(x, y, e1, e2)) =>
      oc.print("\tfcmpd\t%s, %s\n" x y);
      oc.print("\tnop\n");
      g'_non_tail_if oc (NonTail(z)) e1 e2 "fbe" "fbne"
  case (NonTail(z), IfFLE(x, y, e1, e2)) =>
      oc.print("\tfcmpd\t%s, %s\n" x y);
      oc.print("\tnop\n");
      g'_non_tail_if oc (NonTail(z)) e1 e2 "fble" "fbg"
  (* 関数呼び出しの仮想命令の実装 (caml2html: emit_call) *)
  case (Tail, CallCls(x, ys, zs)) => (* 末尾呼び出し (caml2html: emit_tailcall) *)
      g'_args oc [(x, reg_cl)] ys zs;
      oc.print("\tld\t[%s + 0], %s\n" reg_cl reg_sw);
      oc.print("\tjmp\t%s\n" reg_sw);
      oc.print("\tnop\n")
  case (Tail, CallDir(Id.L(x), ys, zs)) => (* 末尾呼び出し *)
      g'_args oc [] ys zs;
      oc.print("\tb\t%s\n" x);
      oc.print("\tnop\n")
  case (NonTail(a), CallCls(x, ys, zs)) =>
      g'_args oc [(x, reg_cl)] ys zs;
      let ss = stacksize () in
      oc.print("\tst\t%s, [%s + %d]\n" reg_ra reg_sp (ss - 4));
      oc.print("\tld\t[%s + 0], %s\n" reg_cl reg_sw);
      oc.print("\tcall\t%s\n" reg_sw);
      oc.print("\tadd\t%s, %d, %s\t! delay slot\n" reg_sp ss reg_sp);
      oc.print("\tsub\t%s, %d, %s\n" reg_sp ss reg_sp);
      oc.print("\tld\t[%s + %d], %s\n" reg_sp (ss - 4) reg_ra);
      if List.mem a allregs && a <> regs.(0) {
         oc.print("\tmov\t%s, %s\n" regs.(0) a)
      } else if List.mem a allfregs && a <> fregs.(0) {
         oc.print("\tfmovs\t%s, %s\n" fregs.(0) a);
         oc.print("\tfmovs\t%s, %s\n" (co_freg fregs.(0)) (co_freg a))
      }
  case (NonTail(a), CallDir(Id.L(x), ys, zs)) =>
      g'_args oc [] ys zs;
      let ss = stacksize () in
      oc.print("\tst\t%s, [%s + %d]\n" reg_ra reg_sp (ss - 4));
      oc.print("\tcall\t%s\n" x);
      oc.print("\tadd\t%s, %d, %s\t! delay slot\n" reg_sp ss reg_sp);
      oc.print("\tsub\t%s, %d, %s\n" reg_sp ss reg_sp);
      oc.print("\tld\t[%s + %d], %s\n" reg_sp (ss - 4) reg_ra);
      if List.mem a allregs && a <> regs.(0) {
        oc.print("\tmov\t%s, %s\n" regs.(0) a)
      } else if List.mem a allfregs && a <> fregs.(0) {
         oc.print("\tfmovs\t%s, %s\n" fregs.(0) a);
         oc.print("\tfmovs\t%s, %s\n" (co_freg fregs.(0)) (co_freg a))
      }
}

def g'_tail_if(oc, e1, e2, b, bn) = {
  val b_else = Id.genid(b + "_else");

  oc.print("\t"+bn+"\t"+b_else+"\n");
  oc.print("\tnop\n");

  val stackset_back = stackset;

  g(oc, (Tail, e1));
  oc.print(b_else+":\n");
  stackset = stackset_back;
  g(oc, (Tail, e2))
}

def g'_non_tail_if(oc, dest, e1, e2, b, bn) = {
  val b_else = Id.genid(b + "_else");
  val b_cont = Id.genid(b + "_cont");

  oc.print("\t"+bn+"\t"+b_else+"\n");
  oc.print("\tnop\n");
  val stackset_back = stackset;

  g(oc, (dest, e1));

  val stackset1 = stackset;

  oc.print("\tb\t%s\n" b_cont);
  oc.print("\tnop\n");
  oc.print("%s:\n" b_else);

  stackset = stackset_back;

  g(oc, (dest, e2));

  oc.print("%s:\n" b_cont);

  val stackset2 = stackset;
  stackset = S.inter(stackset1, stackset2)
}

def g'_args(oc, x_reg_cl, ys, zs) = {
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
      oc.print("\tfmovs\t%s, %s\n", z, fr);
      oc.print("\tfmovs\t%s, %s\n", co_freg(z), co_freg(fr));
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
    data;
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
