functor Main(structure M : PARSERMONOID) =
struct


  fun parseString str =
    let
        val seq = Seq.map M.ofChar (Seq.fromList (String.explode str))
    in
        Seq.reduce M.<@@> M.id seq
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

        val _ = print (M.toString res)
        val _ = print "\n"

        val _ = print (if M.validate res
                    then "✓ accept\n"
                    else "✗ reject\n")
    in
        ()
    end

end