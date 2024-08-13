module type S = sig
  type inactive
  type active
  type activation

  val color : active -> Color.t
  val use : inactive -> activation -> active
end

module Simple : S with type inactive = Color.t with type activation = unit =
struct
  type inactive = Color.t
  type active = Color.t
  type activation = unit

  let color x = x
  let use card _ = card
end

module Dual : S = struct
  type inactive = Dual.t
  type activation = Dual.choice
  type active = inactive * activation

  let color = Dual.color
  let use card choice = (card, choice)
end

module Wild : S = struct
  type inactive = unit
  type activation = Color.t
  type active = Color.t

  let color x = x
  let use _ color = color
end

type card =
  | Simple of Simple.inactive
  | Dual of Dual.inactive
  | Wild of Wild.inactive

type t =
  | Simple of Simple.active
  | Dual of Dual.active
  | Wild of Wild.active

let use card _ = Simple.use card ()
