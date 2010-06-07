package syntax;
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
case class Let(a:(id.T, typ.T), c:T, b:T) extends T
case class Var(b:id.T) extends T
case class LetRec(a:Fundef, b:T) extends T
case class App(a:T, b:List[T]) extends T
case class Tuple(a:List[T]) extends T
case class LetTuple(a:List[(id.T,typ.T)] ,b:T, c:T) extends T
case class Array(a:T, b:T) extends T
case class Get(a:T, b:T) extends T
case class Put(a:T, b:T, c:T) extends T
case class Fundef(name:(id.T, typ.T), args:List[(id.T,typ.T)], body:T)

class Syntax {
	def addtyp(x:id.T):(id.T,typ.T) = (x, typ.Type.gentyp ())

	def tuple2(a:id.T,b:typ.T):(id.T,typ.T) = (a, b)
	def list(a:T):List[T] = List(a)
	def addList(a:T,b:List[T]):List[T] = a :: b
	def addList2(a:(id.T,typ.T),b:List[(id.T,typ.T)]):List[(id.T,typ.T)] = a :: b
	def list2(a:(id.T,typ.T)):List[(id.T,typ.T)] = List(a)
	def concatList2(a:List[(id.T,typ.T)],b:List[(id.T,typ.T)]):List[(id.T,typ.T)] = a ::: b
	def concatList(a:List[T],b:List[T]):List[T] = a ::: b
}
