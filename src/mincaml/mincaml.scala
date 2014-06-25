package mincaml;

class Parse extends Syntax {
	def g(x:Any):T = x match {
		case () => Unit()
		case a:Boolean => Bool(a)
		case a:scala.Int => Int(a)
		case a:scala.Float => Float(a)
		case ("!", a) => Not(g(a))
		case ("-", a) => Neg(g(a))
		case (a, "+", b) => Add(g(a),g(b))
		case (a, "-", b) => Sub(g(a),g(b))
		case (".-", a) => FNeg(g(a))
		case (a, ".+", b) => FAdd(g(a),g(b))
		case (a, ".-", b) => FSub(g(a),g(b))
		case (a, ".*", b) => FMul(g(a),g(b))
		case (a, "./", b) => FDiv(g(a),g(b))
		case (a, "==", b) => Eq(g(a), g(b))
		case (a, "!=", b) => LE(g(a), g(b))
		case ("if", "(", a, ")", (b, "else", c)) => If(g(a), g(b), g(c))
		case ("if", "(", a, ")", b) => If(g(a), g(b), Unit())
		case ("let", (a:String, "=", b)) => Let((a, Type.gentyp()), g(a), g(b))
		case b:String => Var(b)
		case ("letrec", ((name:String,"(",p,")"), "=", body)) =>
			LetRec(
				Fundef(
					(name,Type.gentyp()),
					paramlist(p),
					g(body)
				), g(b)
			)
		case (a, "=", b) => App(g(a), g(b))
		case ("(", r@(a, ",", b) , ")") => Tuple(tuplelist(r))
		case ("let",( ("(",a,")"), "=", b)) => LetTuple((paramlist(a) ,b), c)
		case (a, "=", b) => Array(g(a), g(b))
		case (a, "=", b) => Get(g(a), g(b))
		case (a, "=", b) => Put(g(a), g(b), g(c))
	}
	def paramlist(x:Any):List[(Id.T,Type.T)] = x match {
		case a:String  => List((a,Var(None)))
		case (a,",",b) => paramlist(a) ::: paramlist(b)
		case _         => throw new Exception("error")
	}
	def tuplelist(x:Any):List[Var] = x match {
		case (a, ",", b) => paramlist(a) ::: paramlist(b)
		case a => List(g(a))
	}
}
