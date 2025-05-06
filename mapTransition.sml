signature SEMIGROUP =
sig
  type t
  val <@@> : t * t -> t
end


structure MapSemigroup : SEMIGROUP =
struct
  infix 1 |>
  fun x |> f = f x

  type t = (int * int) list

  fun product ((l : 'a list), (r : 'b list)) : ('a * 'b) list =
    List.concat (List.map (fn x => List.map (fn y => (x, y)) r) l)
  
  fun unique (l : t) : t =
    List.foldr (fn (x, acc) => if List.exists (fn y => x = y) acc then acc else x :: acc) [] l

  fun combineMaps (l : t, r : t) : t =
    product (l, r) |>
    List.filter (fn ((a, b), (c, d)) => b = c) |>
    List.map (fn ((a, b), (c, d)) => (a, d)) |>
    unique

  infix 7 <@@>
  val op <@@> = combineMaps
end


functor MonoidOfSemigroup (structure S : SEMIGROUP) : MONOID =
struct
  type t = S.t option
  val id = NONE
  
  fun combine (NONE, x) = x
    | combine (x, NONE) = x
    | combine (SOME x, SOME y) = SOME (S.<@@> (x, y))

  infix 7 <@@>
  val op <@@> = combine
end


structure MapMonoid = MonoidOfSemigroup (structure S = MapSemigroup)
