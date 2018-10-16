object Main {
  def main(args: Array[String]): Unit = {
    val ParseMe = new CICOM_Parser
    var CICOMCode = "" +
      "let" +
      "  f := map n to if n = 0 then 1 else n * f(n - 1);" +
      "in" +
      "  let" +
      "    f := map n,m,k to if (n <= 0 & n >= 0)" +
      "                  | (n < 0 & n > 0 & n != 0) then number?" +
      "                                           else m / f(k + 1);" +
      "  in" +
      "     let x:=3;" +
      "         y:=4;" +
      "         z:=cons?(function?(x * ~y), cons(-arity(x)));" +
      "     in" +
      "        let x:=3;" +
      "            y:=4;" +
      "            z:=g();" +
      "        in" +
      "            (g(x,y,z))(null?(true),list?(false),first(null))"
    println(CICOMCode+"\nNow parsing: ")
    println(ParseMe.parseAll(ParseMe.Exp, CICOMCode))
  }
}
