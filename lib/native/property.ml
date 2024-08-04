type simple = Color.t
type dual = Dual.t

type card =
  | Simple of simple
  | Dual of dual
  | Wild

type t =
  | Simple of Color.t
  | Dual of Dual.t * Dual.choice
  | Wild of Color.t

let color (card : t) =
  match card with
  | Simple color -> color
  | Dual (colors, choice) -> Dual.color (colors, choice)
  | Wild color -> color

let use_simple color = Simple color
let use_dual colors choice = Dual (colors, choice)
let use_wild color = Wild color
