structure ConcatM : MONOID = 
struct
  type t = string
  val id = ""
  val op @@ = fn (x, y) => x ^ y
end

structure OrM : MONOID =
struct
  type t = bool
  val id = false
  val op @@ = fn (x, y) => x orelse y
end

functor ProductMonoid (
    structure M1 : MONOID
    structure M2 : MONOID) : MONOID =
struct
  type t = M1.t * M2.t
  val id = (M1.id, M2.id)
  val op @@ = fn ((x1, y1), (x2, y2)) => (M1.@@ (x1, x2), M2.@@ (y1, y2))
end


structure TagKindM = ProductMonoid
  (structure M1 = OrM
   structure M2 = ConcatM)


structure XMLToken : TOKEN =
struct
  type t = string
  val eq = op =

  fun ofString s = NONE
  fun toString (s, Left) = "</" ^ s ^ ">"
    | toString (s, Right) = "<" ^ s ^ ">"
end


structure XMLTagMonoid : STACKPARSER =
  MakeStackParserMonoid
    (structure Tok = XMLToken)


(* structure XMLLexingMonoid : TWOLEVELPARSER =
  TwoLevelParser (struct
    structure LexM = TagKindM
    structure ParseM = XMLTagMonoid
    val lex = fn _ => raise Fail "XMLLexingMonoid.lex: not implemented"
  end) *)

structure XMLLexingMonoid : TWOLEVELPARSER =
  TwoLevelParser (struct
    structure LexM = TagKindM
    structure ParseM = XMLTagMonoid

    val lex = fn (close, text) =>
      if text = "" then
        ParseM.ofTok NONE
      else
        ParseM.ofTok (SOME (text, if close then Right else Left))
  end)


structure XMLParser : PARSERMONOID =
struct
  structure X = XMLLexingMonoid
  structure M : PARSERMONOID = struct open XMLLexingMonoid end
  structure MapP = WeightedMapParser (structure WeightP = M)

  open MapP

  val body = 0
  val tagStart = 1
  val nameOpen = 2
  val nameClose = 3
  val afterSpace = 4

  fun ofChar c =
    case c of
      #"<" => SOME [(body, X.break, tagStart)]
    | #"/" => SOME [(tagStart, X.L((true, "")), nameClose)]
    | #">" => SOME [(nameClose, X.id, body), (nameOpen, X.id, body), (afterSpace, X.id, body)]
    | #" " => SOME [(nameOpen, X.id, afterSpace), (nameClose, X.id, afterSpace), (afterSpace, X.id, afterSpace), (body, X.id, body)]
    | c    => SOME [(body, X.id, body), (afterSpace, X.id, afterSpace), (tagStart, X.L((false, Char.toString c)), nameOpen), (nameOpen, X.L((false, Char.toString c)), nameOpen), (nameClose, X.L((false, Char.toString c)), nameClose)]
   
  val leftEnd = SOME [(~1, X.break, body)]
  val rightEnd = SOME [(body, X.break, ~1)]

end

structure Main = Main(structure M = XMLParser)
val _ = Main.main ()