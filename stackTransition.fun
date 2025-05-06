functor MakeStackTransition
          (structure Tok  : TOKEN
           structure Interior : PARSERMONOID)
        : PARSERMONOID =
struct
  datatype t =
      Error
    | Tr of Tok.t Seq.t * Interior.t * Tok.t Seq.t    (* opens, closes *)

  val id = Tr (Seq.empty (), Interior.id, Seq.empty ())

  fun openTag  s = Tr (Seq.empty (),    Interior.id, Seq.singleton s)
  fun closeTag s = Tr (Seq.singleton s, Interior.id, Seq.empty   ())

  fun ofTok (SOME (s, Tok.Left))  = openTag s
    | ofTok (SOME (s, Tok.Right)) = closeTag s
    | ofTok (NONE)                = id

  fun ofChar c =
    let 
      val tok = Tok.ofString (Char.toString c)
    in
      ofTok tok
    end

  fun toString (Error) = "Error"
    | toString (Tr (opens, interior, closes)) =
        let
          val openStr  = Seq.foldr (fn (x, y) => x ^ y) "" (Seq.map (fn x => Tok.toString (x, Tok.Right)) opens)
          val closeStr = Seq.foldr (fn (x, y) => x ^ y) "" (Seq.map (fn x => Tok.toString (x, Tok.Left)) closes)
        in
          openStr ^ " " ^ Interior.toString interior ^ " " ^ closeStr
        end
  
  fun validate (Error) = false
    | validate (Tr (opens, interior, closes)) =
        if (Seq.length opens = 0) andalso
           (Seq.length closes = 0) andalso
           (Interior.validate interior) then
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
          SOME (Seq.drop closes n, Interior.id, Seq.drop opens n)
        else NONE
      end

  fun compose (Error, _)             = Error
    | compose (_, Error)              = Error
    | compose (Tr (lc, lint, lo), Tr (rc, rint, ro)) =
        (case reduce (lo, rc) of
           NONE => Error
         | SOME (closeEx, interior, openEx) =>
             Tr (Seq.append (lc, closeEx),
                    Interior.<@@> (lint, Interior.<@@> (interior, rint)),
                 Seq.append (ro,  openEx)))

  infix 7 <@@>
  val op <@@> = compose
end
