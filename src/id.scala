package mincaml;

object Id {
	case class T(s:String) // 変数の名前
	case class L(l:String) // トップレベル関数やグローバル配列のラベル


	def pp_list(x:List[T]):String = x match {
	case List() => ""
	case List(x) => x.s
	case x :: xs => x + " " + pp_list(xs)
	}

	var counter = 0
	def genid(s:Id.T):Id.T = {
		counter += 1
		T(s.s + "." + counter)
	}

	def id_of_typ(x:Type.T):String = x match {
	case Type.Unit() => "u"
	case Type.Bool() => "b"
	case Type.Int() => "i"
	case Type.Float() => "d"
	case Type.Fun(_,_) => "f"
	case Type.Tuple(_) => "t"
	case Type.Array(_) => "a" 
	case Type.Var(_) => throw new Exception("false")
	}

	def gentmp(tp:Type.T):T = {
		counter += 1
		var rc = T("T" + id_of_typ(tp) + counter)
		println("gentmp rc="+rc);
		return rc;
	}
}
