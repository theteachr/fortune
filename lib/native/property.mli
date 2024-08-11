type simple = Color.t
type dual = Dual.t

(** [card] is inactive and lives in either a player's hand or in the deck. *)
type card =
  | Simple of simple
  | Dual of dual
  | Wild

type _ op =
  | Simple_op : Color.t -> Color.t op
  | Dual_op : dual * Dual.choice -> (dual * Dual.choice) op
  | Wild : Color.t -> Color.t op

(** [t] represents a card that is currently used by a player (in their properties section). *)
type t =
  | Simple of simple
  | Dual of dual * Dual.choice
  | Wild of Color.t

val color : t -> Color.t
val use_simple : simple -> t
val use_dual : Dual.t -> Dual.choice -> t
val use_wild : Color.t -> t
val use : card -> _ op -> t
