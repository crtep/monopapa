datatype side = Left | Right

signature TOKEN =
sig
  type t
  val eq : t * t -> bool          
  
  (* datatype side = Left | Right *)
  
  val ofString : string -> (t * side) option
  val toString : t * side -> string
end


signature STACKMONOID =
sig
  type t
  val id : t
  val @@ : t * t -> t            

  type token

  val ofTok : (token * side) option -> t
  val toString : t  -> string
  val validate : t -> bool
end


