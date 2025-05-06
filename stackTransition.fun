functor MakeStackTransition
          (structure Tok  : TOKEN)
        : MONOID =
struct
  datatype t =
      Error
    | Tr of Tok.t Seq.t * Tok.t Seq.t    (* opens, closes *)

  val id = Tr (Seq.empty (), Seq.empty ())

  fun openTag  s = Tr (Seq.empty (),     Seq.singleton s)
  fun closeTag s = Tr (Seq.singleton s,  Seq.empty   ())

  fun ofChar #"(" = openTag "("
    | ofChar #")" = closeTag "("
    | ofChar #"{" = openTag "{"
    | ofChar #"}" = closeTag "{"
    | ofChar #"[" = openTag "["
    | ofChar #"]" = closeTag "["
    | ofChar _    = id
  
  fun toString (Error) = "Error"
    | toString (Tr (opens, closes)) =
        let
          val openStr  = Seq.foldr (fn (x, y) => x ^ y) "" opens
          val closeStr = Seq.foldr (fn (x, y) => x ^ y) "" closes
        in
          openStr ^ " ... " ^ closeStr
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
        val n      = Int.min (Seq.length opens, Seq.length closes)
        val same =
             Seq.zipWith Tok.eq
               (Seq.take opens n, Seq.take closes n)
        val ok  = Seq.reduce (fn (x,y) => x andalso y) true same
      in
        if ok then
          SOME (Seq.drop opens n, Seq.drop closes n)
        else NONE
      end

  fun compose (Error, _)             = Error
    | compose (_, Error)             = Error
    | compose (Tr (lc, lo), Tr (rc, ro)) =
        (case reduce (lo, rc) of
           NONE => Error
         | SOME (openEx, closeEx) =>
             Tr (Seq.append (lc, closeEx),
                 Seq.append (ro,  openEx)))

  infix 7 <@@>
  val op <@@> = compose
end
