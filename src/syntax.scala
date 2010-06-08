package mincaml;

class Syntax {
	// MinCamlの構文を表現するデータ型 (caml2html: Syntax)
	sealed abstract case class T()
	case class Unit() extends T
	case class Bool(a:Boolean) extends T
	case class Int(a:scala.Int) extends T
	case class Float(a:Double) extends T
	case class Not(a:T) extends T
	case class Neg(a:T) extends T
	case class Add(a:T, b:T) extends T
	case class Sub(a:T, b:T) extends T
	case class FNeg(a:T) extends T
	case class FAdd(a:T, b:T) extends T
	case class FSub(a:T, b:T) extends T
	case class FMul(a:T, b:T) extends T
	case class FDiv(a:T, b:T) extends T
	case class Eq(a:T, b:T) extends T
	case class Le(a:T, b:T) extends T
	case class If(a:T, b:T, c:T) extends T
	case class Let(a:(Id.T, Type.T), c:T, b:T) extends T
	case class Var(b:Id.T) extends T
	case class LetRec(a:Fundef, b:T) extends T
	case class App(a:T, b:List[T]) extends T
	case class Tuple(a:List[T]) extends T
	case class LetTuple(a:List[(Id.T,Type.T)] ,b:T, c:T) extends T
	case class Array(a:T, b:T) extends T
	case class Get(a:T, b:T) extends T
	case class Put(a:T, b:T, c:T) extends T
	case class Fundef(name:(Id.T, Type.T), args:List[(Id.T,Type.T)], body:T)

	def addtyp(x:Id.T):(Id.T,Type.T) = (x, Type.gentyp ())

	def tuple2(a:Id.T,b:Type.T):(Id.T,Type.T) = (a, b)
	def list(a:T):List[T] = List(a)
	def addList(a:T,b:List[T]):List[T] = a :: b
	def addList2(a:(Id.T,Type.T),b:List[(Id.T,Type.T)]):List[(Id.T,Type.T)] = a :: b
	def list2(a:(Id.T,Type.T)):List[(Id.T,Type.T)] = List(a)
	def concatList2(a:List[(Id.T,Type.T)],b:List[(Id.T,Type.T)]):List[(Id.T,Type.T)] = a ::: b
	def concatList(a:List[T],b:List[T]):List[T] = a ::: b
}
