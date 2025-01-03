type t

val count : t -> int
val shuffle : ?seed:int -> t -> t
val default : t
val take : int -> t -> Card.t list * t
val take' : int -> t -> Card.t list * (int, t) Either.t
val add : Card.t -> t -> t
val of_list : Card.t list -> t
