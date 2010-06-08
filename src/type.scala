package mincaml;
object Type {
	sealed abstract case class T()
	case class Unit() extends T
	case class Bool() extends T
	case class Int() extends T
	case class Float() extends T
	case class Fun(a:List[T], b:T) extends T // arguments are uncurried
	case class Tuple(a:List[T]) extends T
	case class Array(a:T) extends T
	case class Var(var a:Option[T]) extends T

	def gentyp():T = Var(None) // 新しい型変数を作る
}
