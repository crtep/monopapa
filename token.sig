signature TOKEN =
sig
  type t
  val eq : t * t -> bool           (* equality on the token type *)
end
