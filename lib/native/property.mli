type simple = Color.t
type dual = Dual.t
type wild

(** [card] is inactive and lives in either a player's hand or in the deck. *)
type card =
  | Simple of simple
  | Dual of dual
  | Wild of wild

(** [t] represents a card that is currently used by a player (in their properties section). *)
type t =
  | Simple of Color.t
  | Dual of Dual.t * Dual.choice
  | Wild of Color.t

val color : t -> Color.t
val wild : card

val use :
  ?color:Color.t -> card -> (t, [> `Invalid_color | `Missing_color ]) result
