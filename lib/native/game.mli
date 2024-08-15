type t

val start : Player.t list -> t option
val current_player : t -> Player.t
val play_card : t -> Card.t -> t
