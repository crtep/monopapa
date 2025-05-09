functor MakeStackMonoid
          (structure Tok  : TOKEN)
        : STACKMONOID =
struct
  datatype t =
      Error
    | Tr of Tok.t Seq.t * Tok.t Seq.t    (* opens, closes *)
  
  type token = Tok.t

  val id = Tr (Seq.empty (), Seq.empty ())

  fun openTag  s = Tr (Seq.empty (),    Seq.singleton s)
  fun closeTag s = Tr (Seq.singleton s, Seq.empty   ())

  fun ofTok (SOME (s, Left))  = openTag s
    | ofTok (SOME (s, Right)) = closeTag s
    | ofTok (NONE)            = id

  fun ofString s =
    let 
      val tok = Tok.ofString (s)
    in
      ofTok tok
    end

  fun toString (Error) = "Error"
    | toString (Tr (opens, closes)) =
        let
          val openStr  = Seq.foldr (fn (x, y) => x ^ y) "" (Seq.map (fn x => Tok.toString (x, Right)) opens)
          val closeStr = Seq.foldr (fn (x, y) => x ^ y) "" (Seq.map (fn x => Tok.toString (x, Left)) closes)
        in
          openStr ^ " " ^ closeStr
        end
  
  fun validate (Error) = false
    | validate (Tr (opens, closes)) =
        if (Seq.length opens = 0) andalso
           (Seq.length closes = 0) then
          true
        else
          false

  fun reduce (opens, closes) =
      let
        val n    = Int.min (Seq.length opens, Seq.length closes)
        val same =
             Seq.zipWith Tok.eq
               (Seq.take opens n, Seq.take closes n)
        val ok  = Seq.reduce (fn (x,y) => x andalso y) true same
      in
        if ok then
          SOME (Seq.drop closes n, Seq.drop opens n)
        else NONE
      end

  fun compose (Error, _)             = Error
    | compose (_, Error)              = Error
    | compose (Tr (lc,  lo), Tr (rc,  ro)) =
        (case reduce (lo, rc) of
           NONE => Error
         | SOME (closeEx, openEx) =>
             Tr (Seq.append (lc, closeEx),
                 Seq.append (ro,  openEx)))

  infix 7 @@
  val op @@ = compose
end

signature STACKPARSER = 
sig
  type t
  val id : t
  val @@ : t * t -> t            

  type token

  val ofTok : (token * side) option -> t
  val toString : t  -> string
  val validate : t -> bool
  val leftEnd : t
  val rightEnd : t
  val ofChar : char -> t
end


functor MakeStackParserMonoid
          (structure Tok  : TOKEN)
        : STACKPARSER =
struct
  structure M = MakeStackMonoid (
    structure Tok = Tok)

  open M

  val id = M.id
  val toString = M.toString
  val validate = M.validate
  val op @@ = M.@@

  val leftEnd = id
  val rightEnd = id

  fun ofChar c =
    let
      val tok = Tok.ofString (Char.toString c)
    in
      ofTok tok
    end
end

functor MakeStackTransition
          (structure Tok  : TOKEN)
        : PARSERMONOID =
struct
  structure M = MakeStackParserMonoid (
    structure Tok = Tok)

  open M
end