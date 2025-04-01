type 'a t

val count : 'a t -> int
val shuffle : ?seed:int -> 'a t -> 'a t
val default : Card.t t
val take : int -> 'a t -> 'a list * 'a t
val take' : int -> 'a t -> 'a list * (int, 'a t) Either.t
val add : 'a -> 'a t -> 'a t
val of_list : 'a list -> 'a t
