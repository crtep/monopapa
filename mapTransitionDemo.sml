structure MapTransition =
struct
  type t = MapMonoid.t
  val id = MapMonoid.id
  val op <@@> = MapMonoid.<@@>

  fun ofChar c =
    case c of
      #"(" => SOME [(0, 1)]
    | #")" => SOME [(1, 0)]
    | _    => SOME []

  fun toString (NONE) = "id"
  | toString (SOME lst) = String.concat (List.map (fn (x, y) => "(" ^ Int.toString x ^ " -> " ^ Int.toString y ^ ")") lst)

  fun validate (NONE) = true
    | validate (SOME []) = false
    | validate (SOME lst) = true
end



fun parseString str =
  let
    val seq = Seq.map MapTransition.ofChar (Seq.fromList (String.explode str))
  in
    Seq.reduce MapTransition.<@@> MapTransition.id seq
  end 



val () = print "enter string: "

val raw = 
  case TextIO.inputLine TextIO.stdIn of
    NONE => raise Fail "no input"
  | SOME raw => raw


val input     = String.substring (raw, 0, String.size raw - 1)

val res = parseString input

val _ = print (MapTransition.toString res)
val _ = print "\n"

val _ = print (if MapTransition.validate res
               then "✓ accept\n"
               else "✗ reject\n")
