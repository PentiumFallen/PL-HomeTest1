import scala.Function
import scala.util.parsing.combinator._

class CICOM_Parser extends RegexParsers {
  def Reserved:Parser[Any] = "null"|"if"|"then"|"else"|"let"|"in"|"map"|"to"|"number?"|"function?"|"list?"|"empty?"|"cons?"|"cons"|"first"|"rest"|"arity"

  //Tokens
  def Character:Parser[Any] =
    "a"|"e"|"i"|"o"|"u"|"y"|"A"|"E"|"I"|"O"|"U"|"Y"|
      "q"|"w"|"r"|"t"|"p"|"s"|"d"|"f"|"g"|"h"|"j"|"k"|
      "l"|"z"|"x"|"c"|"v"|"b"|"n"|"m"|"Q"|"W"|"R"|"T"|
      "P"|"S"|"D"|"F"|"G"|"H"|"J"|"K"|"L"|"Z"|"X"|"C"|
      "V"|"B"|"N"|"M"|"?"|"_"
  def Digit:Parser[Any] = "0"|"1"|"2"|"3"|"4"|"5"|"6"|"7"|"8"|"9"
  def Delimiters:Parser[Any] = "("|")"|"["|"]"|","|";"
  def Operator:Parser[Any] = "<="|">="|":="|"<"|">"|"+"|"-"|"~"|"*"|"/"|"="|"!="|"&"|"|"

  //Basic types
  def Empty:Parser[Any] = "null" | "empty"
  def Bool:Parser[Any] = "true".r|"false".r
  def Sign:Parser[Any] = "+"|"-"
  def Unop:Parser[Any] = Sign|"~"
  def Binop:Parser[Any] = Sign|"!="|">="|"<="|"*"|"/"|"="|"<"|">"|"&"|"|"
  def Id:Parser[Any] = (not(Reserved)~Character) ~ rep((not(Reserved) ~ Character)|(not(Reserved) ~ Digit))
  def Int:Parser[Any] = rep1(Digit)
  def Prim:Parser[Any] = "number?"|"function?"|"list?"|"empty?"|"cons?"|"cons"|"first"|"rest"|"arity"

  //Grammar
  def Exp:Parser[Any] = Term ~ rep(Binop ~ Exp) |
    "if" ~ Exp ~ "then" ~ Exp ~ "else" ~ Exp |
    "let" ~ rep1(Def) ~ "in" ~ Exp |
    "map" ~ IdList ~ "to" ~ Exp
  def Term:Parser[Any] = Unop ~ Term |
    Empty |
    Int |
    Bool |
    Factor ~ rep("(" ~ ExpList ~ ")")
  def Factor:Parser[Any] = "(" ~ Exp ~ ")" |
    Prim |
    Id
  def ExpList:Parser[Any] = rep(PropExpList)
  def PropExpList:Parser[Any] = Exp ~ "," ~ PropExpList |
    Exp
  def IdList:Parser[Any] = PropIdList
  def PropIdList:Parser[Any] = Id ~ "," ~ PropIdList |
    Id
  def Def:Parser[Any] = Id ~ ":=" ~ Exp ~ ";"
}
