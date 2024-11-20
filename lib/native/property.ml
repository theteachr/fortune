type simple = Color.t
type dual = Dual.t
type wild = unit

type card =
  | Simple of simple
  | Dual of dual
  | Wild of wild

type t =
  | Simple of Color.t
  | Dual of Dual.active
  | Wild of Color.t

let color (card : t) =
  match card with
  | Simple color -> color
  | Dual (colors, choice) -> Dual.color (colors, choice)
  | Wild color -> color

let use_simple color = Simple color
let use_dual colors choice = Dual (colors, choice)
let use_wild _ color = Wild color
let wild : card = Wild ()
