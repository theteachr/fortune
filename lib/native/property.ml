type simple = Color.t
type dual = Dual.t

type card =
  | Simple of simple
  | Dual of dual
  | Wild

type _ op =
  | Simple_op : Color.t -> simple op
  | Dual_op : dual * Dual.choice -> (dual * Dual.choice) op
  | Wild : Color.t -> Color.t op

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

let use : type input. card -> input op -> t =
 fun _card op ->
  match op with
  | Simple_op color -> Simple color
  | Dual_op (dual, choice) -> Dual (dual, choice)
  | Wild color -> Wild color
