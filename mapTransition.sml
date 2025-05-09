signature SEMIGROUP =
sig
  type t
  val @@ : t * t -> t
end

infix 1 |>
fun x |> f = f x


fun product ((l : 'a list), (r : 'b list)) : ('a * 'b) list =
  List.concat (List.map (fn x => List.map (fn y => (x, y)) r) l)

fun unique (l : ''a list) : ''a list =
  List.foldr (fn (x, acc) => if List.exists (fn y => x = y) acc then acc else x :: acc) [] l


structure MapSemigroup : SEMIGROUP =
struct
  type t = (int * int) list

  fun combineMaps (l : t, r : t) : t =
    product (l, r) |>
    List.filter (fn ((a, b), (c, d)) => b = c) |>
    List.map (fn ((a, b), (c, d)) => (a, d)) |>
    unique

  infix 7 @@
  val op @@ = combineMaps
end


functor WeightedMapSemigroup (structure WeightS : SEMIGROUP) : SEMIGROUP =
struct
  type t = (int * WeightS.t * int) list

  fun uniqueOuter (l : t) : t =
    List.foldr (fn ((a, x, b), acc) => if List.exists (fn (c, y, d) => a = c andalso b = d) acc then acc else (a, x, b) :: acc) [] l

  fun combineMaps (l : t, r : t) : t =
    product (l, r) |>
    List.filter (fn ((a, x, b), (c, y, d)) => b = c) |>
    List.map (fn ((a, x, b), (c, y, d)) => (a, WeightS.@@ (x, y), d)) |>
    uniqueOuter

  infix 7 @@
  val op @@ = combineMaps
end


functor MonoidOfSemigroup (structure S : SEMIGROUP) : MONOID =
struct
  type t = S.t option
  val id = NONE
  
  fun combine (NONE, x) = x
    | combine (x, NONE) = x
    | combine (SOME x, SOME y) = SOME (S.@@ (x, y))

  infix 7 @@
  val op @@ = combine
end

(* functor SemigroupOfMonoid (structure M : MONOID) : SEMIGROUP =
struct
  open M
end *)

structure MapMonoid = MonoidOfSemigroup (structure S = MapSemigroup)

functor WeightedMapMonoid (structure WeightS : SEMIGROUP) : MONOID = MonoidOfSemigroup (structure S = WeightedMapSemigroup (structure WeightS = WeightS))

functor WeightedMapParser (structure WeightP : PARSERMONOID) : PARSERMONOID =
struct
  structure WeightS : SEMIGROUP =
  struct
    open WeightP
  end

  structure MapM = WeightedMapMonoid (structure WeightS = WeightS)
  open MapM

  fun ofChar c = raise Fail "ofChar not implemented"
  val leftEnd = NONE
  val rightEnd = NONE

  fun getInterior (NONE) = SOME (WeightP.id)
    | getInterior (SOME ((a, x, b)::_)) = SOME x
    | getInterior (SOME []) = NONE
  
  fun toString x =
    case getInterior x of
      NONE => "Lexer error!" 
    | SOME value => (WeightP.toString value)  

  fun validate x =
    case getInterior x of
      NONE => false
    | SOME value => (WeightP.validate value)
    
end