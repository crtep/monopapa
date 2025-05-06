structure MapTransition =
struct
  type t = MapMonoid.t
  val id = MapMonoid.id
  val op <@@> = MapMonoid.<@@>

  fun ofChar c =
    case c of
      #"(" => SOME [(~1, 0), (~1, 1), (~1, 2), (~1, 3)]
    | #")" => SOME [(0, ~1), (1, ~1), (2, ~1), (3, ~1)]
    | #"0" => SOME [(0, 0), (2, 0), (3, 3)]
    | #"1" => SOME [(0, 1), (1, 2), (1, 3), (2, 1), (3, 2)]
    | _    => NONE (* ignore other characters *)

  fun toString (NONE) = "id"
  | toString (SOME lst) = String.concat (List.map (fn (x, y) => Int.toString x ^ " -> " ^ Int.toString y ^ "\n") lst)

  fun validate (NONE) = true
    | validate (SOME []) = false
    | validate (SOME lst) = true
end


structure MapMain = Main(structure M = MapTransition)
val _ = MapMain.main ()