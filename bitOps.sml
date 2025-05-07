fun wrapInt (f: (Word.word * Word.word) -> Word.word) ((x:int), (y:int)) : int =
    let
        val wx = Word.fromInt x
        val wy = Word.fromInt y
        val result = f (wx, wy)
    in
        Word.toInt result
    end


infix 7 ~>>
val op ~>> = wrapInt Word.~>>

infix 7 <<
val op << = wrapInt Word.<<

infix 7 &&&
val op &&& = wrapInt Word.andb

infix 7 |||
val op ||| = wrapInt Word.orb