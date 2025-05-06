signature TOKEN =
sig
  type t
  val eq : t * t -> bool          
  
  datatype side = Left | Right
  val ofString : string -> (t * side) option
  val toString : t * side -> string
end
