signature PARSERMONOID =
sig
  type t
  val id   : t
  val @@  : t * t -> t            

  val ofChar : char -> t
  val toString : t -> string
  val validate : t -> bool

  val leftEnd : t
  val rightEnd : t
end


signature MONOID =
sig
  type t
  val id   : t
  val @@  : t * t -> t            
end


structure Nil : MONOID =
struct
  type t = unit
  val id = ()
  val op @@ = fn (x, y) => ()
end


functor ParserOfMonoid (structure M : MONOID) : PARSERMONOID =
struct
  type t = M.t
  val id = M.id


  infix 7 @@
  val op @@ = M.@@

  fun ofChar _ = id
  fun toString _ = ""
  fun validate _ = true

  val leftEnd = id
  val rightEnd = id
end

structure NilParser = ParserOfMonoid (structure M : MONOID = Nil)