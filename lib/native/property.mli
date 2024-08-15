module Simple : sig
  type t

  val color : t -> Color.t
end

module DualWild : sig
  type t

  val colors : t -> Color.t * Color.t
end

module Wild : sig
  type t
end

type card =
  | Simple of Simple.t
  | Dual of DualWild.t
  | Wild

type inactive
type active

type _ t =
  | Inactive : card -> inactive t
  | Simple : Simple.t -> active t
  | Dual : DualWild.t * Dual.choice -> active t
  | Wild : Color.t -> active t

val color : active t -> Color.t
val use_simple : Simple.t -> active t
val use_dual : DualWild.t -> Dual.choice -> active t
val use_wild : Color.t -> active t
