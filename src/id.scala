package id;
	case class T(s:String) // 変数の名前
	case class L(l:String) // トップレベル関数やグローバル配列のラベル

object Id {
	def pp_list(x:List[T]):String = x match {
	case List() => ""
	case List(x) => x.s
	case x :: xs => x + " " + pp_list(xs)
	}

	var counter = 0
	def genid(s:String):String = {
		counter += 1
		s + "." + counter
	}

	def id_of_typ(x:typ.T):String = x match {
	case typ.Unit() => "u"
	case typ.Bool() => "b"
	case typ.Int() => "i"
	case typ.Float() => "d"
	case typ.Fun(_,_) => "f"
	case typ.Tuple(_) => "t"
	case typ.Array(_) => "a" 
	case typ.Var(_) => throw new Exception("false")
	}

	def gentmp(tp:typ.T):T = {
		counter += 1
		var rc = T("T" + id_of_typ(tp) + counter)
		println("gentmp rc="+rc);
		return rc;
	}
}
