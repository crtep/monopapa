structure RelMap : MONOID =
struct
  type t = (int * int) list

  (* fun member x []      = false
    | member x (y::ys) = x = y orelse member x ys

  fun addU (x, xs) = if member x xs then xs else x :: xs
  fun union (xs, ys) = List.foldl addU xs ys *)

  fun lookup (m:t) k =
    let
      val pairs = List.filter (fn (k',_) => k' = k) m
      val vals  = List.map (fn (_,v) => v) pairs
    in
      vals
    end
  
  fun lookup_mult (m:t) ks =
    let
      fun step (k, acc) =
            let val v = lookup m k
            in  v @ acc end
      val pairs = List.map (fn k => (k, [])) ks
      val vals  = List.foldl step [] pairs
    in
      List.foldl (fn (x, acc) => x @ acc) [] vals
    end


  fun combineMaps (l : t, r : t) : t =
    let
      fun step (left, mids) =
            let val rights = List.foldl (fn (m,acc) => union (acc, lookup r m)) [] mids
            in  (left, rights) end
    in
      List.map step l
    end


  val id = []
  infix 7 <@@>
  val op <@@> = combineMaps
end
