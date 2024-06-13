module Dual = struct
  type t = Color.t * Color.t

  type choice =
    | L
    | R
end

type card =
  | Simple of Color.t
  | Dual of Dual.t
  | Wild

type t =
  | Simple of Color.t
  | Dual of Dual.t * Dual.choice
  | Wild of Color.t
