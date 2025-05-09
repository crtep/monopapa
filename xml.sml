structure ConcatM : MONOID = 
struct
  type t = string
  val id = ""
  val op @@ = fn (x, y) => x ^ y
end

datatype XMLTagGender = Opening | Closing | SelfClosing

structure RightHandedS : SEMIGROUP =
struct
  type t = XMLTagGender
  val op @@ = fn (x, y) => y
end

structure RightHandedM : MONOID = MonoidOfSemigroup
  (structure S = RightHandedS)

functor ProductMonoid (
    structure M1 : MONOID
    structure M2 : MONOID) : MONOID =
struct
  type t = M1.t * M2.t
  val id = (M1.id, M2.id)
  val op @@ = fn ((x1, y1), (x2, y2)) => (M1.@@ (x1, x2), M2.@@ (y1, y2))
end


structure TagKindM = ProductMonoid
  (structure M1 = RightHandedM
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

    val lex = fn (gender, text) => ParseM.ofTok (
      case text of 
        "" => NONE
      | _  => 
        case gender of
          NONE => NONE
        | SOME Opening => SOME (text, Left)
        | SOME Closing => SOME (text, Right)
        | SOME SelfClosing => NONE
    ) end)


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
  val afterFinalSlash = 5
  val quoted = 6

  fun ofChar c =
    case c of
      #"<" => SOME [(body, X.break, tagStart), 
                    (quoted, X.id, quoted)]
    | #"/" => SOME [(tagStart, X.L((SOME Closing, "")), nameClose),
                    (quoted, X.id, quoted), 
                    (nameOpen, X.L((SOME SelfClosing, "")), afterFinalSlash), 
                    (nameClose, X.L((SOME SelfClosing, "")), afterFinalSlash), 
                    (afterSpace, X.L((SOME SelfClosing, "")), afterFinalSlash)]
    | #">" => SOME [(nameClose, X.id, body), 
                    (quoted, X.id, quoted),
                    (nameOpen, X.id, body), 
                    (afterSpace, X.id, body),
                    (afterFinalSlash, X.id, body)]
    | #" " => SOME [(nameOpen, X.id, afterSpace),
                    (quoted, X.id, quoted),
                    (nameClose, X.id, afterSpace), 
                    (afterSpace, X.id, afterSpace), 
                    (body, X.id, body)]
    | #"\"" => SOME [(body, X.id, body),
                    (afterSpace, X.id, quoted),
                    (quoted, X.id, afterSpace)]
    | c    => SOME [(body, X.id, body),
                    (quoted, X.id, quoted),
                    (afterSpace, X.id, afterSpace), 
                    (tagStart, X.L((SOME Opening, Char.toString c)), nameOpen), 
                    (nameOpen, X.L((NONE, Char.toString c)), nameOpen), 
                    (nameClose, X.L((NONE, Char.toString c)), nameClose)]
   
  val leftEnd = SOME [(~1, X.break, body)]
  val rightEnd = SOME [(body, X.break, ~1)]

end

structure Main = Main(structure M = XMLParser)
val _ = Main.main ()