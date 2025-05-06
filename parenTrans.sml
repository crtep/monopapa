(* instantiate the functor for parentheses/XML-style tags *)
structure StringTok : TOKEN =
struct
  type t = string
  val eq = op =
end

structure ParenTrans =
  MakeStackTransition
     (structure Tok = StringTok)


fun parseString str =
  let
    val seq = Seq.map ParenTrans.ofChar (Seq.fromList (String.explode str))
  in
    Seq.reduce ParenTrans.<@@> ParenTrans.id seq
  end



val () = print "enter brackets: "
val SOME raw = TextIO.inputLine TextIO.stdIn
val input     = String.substring (raw, 0, String.size raw - 1)

val res = parseString input

val _ = print (if ParenTrans.validate res
               then "✓ balanced\n"
               else "✗ mismatch\n")
