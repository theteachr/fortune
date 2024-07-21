type t

val count : t -> int
val shuffle : t -> t
val default : t
val take : int -> t -> Card.t list * t
