type inactive
type active

module Simple = struct
  type t = Color.t

  let color x = x
end

module DualWild = struct
  type t = Dual.t

  let colors x = x
end

module Wild = struct
  type t
end

type card =
  | Simple of Simple.t
  | Dual of DualWild.t
  | Wild

type _ t =
  | Inactive : card -> inactive t
  | Simple : Simple.t -> active t
  | Dual : DualWild.t * Dual.choice -> active t
  | Wild : Color.t -> active t

let color = function
  | Simple color -> color
  | Dual (colors, choice) -> Dual.color (colors, choice)
  | Wild color -> color

let use_simple color = Simple color
let use_dual colors choice = Dual (colors, choice)
let use_wild color = Wild color
