signature MONOID =
sig
  type t
  val id   : t
  val <@@>  : t * t -> t            

  val ofChar : char -> t
  val toString : t -> string
  val validate : t -> bool
end


structure Nil : MONOID =
struct
  type t = unit
  val id = ()
  infix 7 <@@>
  val op <@@> = fn (x, y) => ()
  
  fun ofChar _ = ()
  fun toString _ = ""
  fun validate _ = true
end