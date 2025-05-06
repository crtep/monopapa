signature MONOID =
sig
  type t
  val id   : t
  val <@@>  : t * t -> t            

  val ofChar : char -> t
  val toString : t -> string
  val validate : t -> bool
end
