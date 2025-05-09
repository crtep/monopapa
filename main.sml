functor Main(structure M : PARSERMONOID) =
struct
  open M
  infix 7 @@

  fun parseString str =
    let
        val seq = Seq.map ofChar (Seq.fromList (String.explode str))
    in
        leftEnd @@ (Seq.reduce M.@@ id seq) @@ rightEnd
    end 

  fun main _ = 
    let
        val _ = print "enter string: "

        val raw = 
        case TextIO.inputLine TextIO.stdIn of
            NONE => raise Fail "no input"
        | SOME raw => raw


        val input     = String.substring (raw, 0, String.size raw - 1)

        val res = parseString input

        val _ = print (toString res)
        val _ = print "\n"

        val _ = print (if validate res
                    then "✓ accept\n"
                    else "✗ reject\n")
    in
        ()
    end

end