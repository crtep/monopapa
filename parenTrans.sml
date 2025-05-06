(* instantiate the functor for parentheses/XML-style tags *)
structure StringTok : TOKEN =
struct
  type t = string
  val eq = op =

  datatype side = Left | Right

  fun ofString "(" = SOME ("(", Left)
    | ofString ")" = SOME ("(", Right)
    | ofString "{" = SOME ("{", Left)
    | ofString "}" = SOME ("{", Right)
    | ofString "[" = SOME ("[", Left)
    | ofString "]" = SOME ("[", Right)
    | ofString _   = NONE

  fun toString ("(", Left) = "("
    | toString ("(", Right) = ")"
    | toString ("{", Left) = "{"
    | toString ("{", Right) = "}"
    | toString ("[", Left) = "["
    | toString ("[", Right) = "]"
    | toString _ = "?"
end

structure ParenTrans =
  MakeStackTransition
     (structure Tok = StringTok
      structure Interior = NilParser)


fun parseString str =
  let
    val seq = Seq.map ParenTrans.ofChar (Seq.fromList (String.explode str))
  in
    Seq.reduce ParenTrans.<@@> ParenTrans.id seq
  end


structure ParserMain = Main(structure M = ParenTrans)
val _ = ParserMain.main ()