type t

val count : t -> int
val shuffle : ?seed:int -> t -> t
val default : t
val take : int -> t -> Card.t list * t
