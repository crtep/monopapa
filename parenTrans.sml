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
      structure Interior = Nil)


fun parseString str =
  let
    val seq = Seq.map ParenTrans.ofChar (Seq.fromList (String.explode str))
  in
    Seq.reduce ParenTrans.<@@> ParenTrans.id seq
  end



val () = print "enter brackets: "

val raw = 
  case TextIO.inputLine TextIO.stdIn of
    NONE => raise Fail "no input"
  | SOME raw => raw


val input     = String.substring (raw, 0, String.size raw - 1)

val res = parseString input

val _ = print (ParenTrans.toString res)
val _ = print "\n"

val _ = print (if ParenTrans.validate res
               then "✓ balanced\n"
               else "✗ mismatch\n")
