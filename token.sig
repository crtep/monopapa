signature TOKEN =
sig
  type t
  datatype side = Left | Right
  val eq : t * t -> bool           (* equality on the token type *)
  (* val ofString : char -> t * side *)
  (* val toString : t * side -> string *)
end
