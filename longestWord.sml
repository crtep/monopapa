structure AddM : MONOID =
struct
  type t = int
  val id = 0
  val op @@ = fn (x, y) => x + y
end

structure MaxM : MONOID =
struct
  type t = int
  val id = 0
  val op @@ = fn (x, y) => Int.max (x, y)
end

structure LongestWordMonoid : TWOLEVELMONOID = TwoLevelMonoid
  (structure LexM = AddM; structure ParseM = MaxM; val lex = (fn x => x));

structure LongestWordParser : PARSERMONOID =
struct
  open LongestWordMonoid

  val leftEnd = break
  val rightEnd = break

  fun ofChar c =
    case c of
      #" " => break
    | _    => L(1)

  fun toString x = Int.toString (parsedPart x)

  fun validate x = true
end