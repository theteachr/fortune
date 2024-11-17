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

(* Activation Functions *)

val use_simple : simple -> t
val use_dual : dual -> Dual.choice -> t
val use_wild : wild -> Color.t -> t
