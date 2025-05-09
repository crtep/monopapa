structure WordMapMonoid = WeightedMapMonoid(structure WeightS = LongestWordMonoid)

structure LongestQuoteTransition : PARSERMONOID =
struct
  open WordMapMonoid

  val body = 0
  val text = 1
  val esc = 2

  val L = LongestWordMonoid.L
  val break = LongestWordMonoid.break
  val parsedPart = LongestWordMonoid.parsedPart

  fun ofChar c =
    case c of
    | #"\"" => SOME [(body, break, text), (text, L(0), body), (esc, L(1), text)]
    | #"\\" => SOME [(text, L(0), esc), (esc, L(1), text)] 
    | _     => SOME [(body, L(0), body), (text, L(1), text)]

  val leftEnd = SOME [(~1, break, body)]
  val rightEnd = SOME [(body, break, ~1)]

  fun toInt (NONE) = SOME 0
    | toInt (SOME ((a, x, b)::xs)) = SOME (parsedPart x)
    | toInt (SOME []) = NONE

  fun toString x =
    case toInt x of
    | NONE => "Error"
    | SOME x => Int.toString x

  fun validate (NONE) = true
    | validate (SOME []) = false
    | validate (SOME lst) = true
end


structure Main = Main(structure M = LongestQuoteTransition)
val _ = Main.main ()