structure CellularAutomatonTransition =
struct
  type t = MapMonoid.t
  val id = MapMonoid.id
  val op @@ = MapMonoid.@@

  val rule = 110

  infix 1 |>
  fun x |> f = f x

  fun edgesOfOutBit outBit : (int * int) list =
    List.tabulate (8, (fn i => i)) |>
      List.filter (fn i => (rule ~>> i) &&& 1 = outBit) |>
      List.map (fn i => ((i ~>> 1) &&& 3, i &&& 3))

  fun ofChar c =
    case c of
      #"(" => SOME [(~1, 0), (~1, 1), (~1, 2), (~1, 3)]
    | #")" => SOME [(0, ~1), (1, ~1), (2, ~1), (3, ~1)]
    | #"0" => SOME (edgesOfOutBit 0)
    | #"1" => SOME (edgesOfOutBit 1)
    | _    => NONE (* ignore other characters *)

  fun toString (NONE) = "id"
  | toString (SOME lst) = String.concat (List.map (fn (x, y) => Int.toString x ^ " -> " ^ Int.toString y ^ "\n") lst)

  fun validate (NONE) = true
    | validate (SOME []) = false
    | validate (SOME lst) = true
end


structure MapMain = Main(structure M = CellularAutomatonTransition)
val _ = MapMain.main ()