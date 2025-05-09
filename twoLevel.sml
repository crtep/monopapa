signature TWOLEVELMONOID =
sig
  type lt
  type pt
  datatype t = L of lt | LPL of lt * pt * lt
  val id : t
  val @@ : t * t -> t

  val break : t
  val parsedPart : t -> pt
end

functor TwoLevelMonoid (structure LexM : MONOID; structure ParseM : MONOID; val lex : LexM.t -> ParseM.t) : TWOLEVELMONOID =
struct
  type lt = LexM.t
  type pt = ParseM.t

  datatype t = L of lt | LPL of lt * pt * lt

  val id = L (LexM.id)
  val break = LPL (LexM.id, ParseM.id, LexM.id)

  fun parsedPart (L x) = ParseM.id
    | parsedPart (LPL (x, y, z)) = y

  infix 7 ##
  infix 7 $$
  val op ## = LexM.@@
  val op $$ = ParseM.@@

  fun combine (L x, L y) = L (x ## y)
    | combine (L x, LPL (y, z, w)) = LPL (x ## y, z, w)
    | combine (LPL (x, y, z), L w) = LPL (x, y, z ## w)
    | combine (LPL (x, y, z), LPL (w, u, v)) =
        LPL (x, y $$ lex(z ## w) $$ u, v)

  infix 7 @@
  val op @@ = combine
end

functor MonoidOfParser (structure P : PARSERMONOID) : MONOID =
struct
  open P
end

signature TWOLEVELPARSER =
sig
  type lt
  type pt
  datatype t = L of lt | LPL of lt * pt * lt
  val id : t
  val @@ : t * t -> t

  val break : t
  val parsedPart : t -> pt
  val ofChar : char -> t
  val toString : t -> string
  val validate : t -> bool
  val leftEnd : t
  val rightEnd : t
end

functor TwoLevelParser (structure LexM : MONOID; structure ParseM : PARSERMONOID; val lex : LexM.t -> ParseM.t) : TWOLEVELPARSER =
struct
  structure M = TwoLevelMonoid (
    structure LexM = LexM
    structure ParseM = MonoidOfParser(structure P = ParseM)
    val lex = lex)
  open M

  fun ofChar c = raise Fail "ofChar not implemented"
  fun toString x = ParseM.toString (parsedPart x)
  fun validate x = ParseM.validate (parsedPart x)

  val leftEnd = break
  val rightEnd = break
end

