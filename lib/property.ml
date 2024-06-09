type t =
  | Simple of Color.t
  | Dual of Color.t * Color.t
  | Wild

let show = function
  | Simple color -> Color.show color
  | Dual (a, b) -> Printf.sprintf "%s%s" (Color.show a) (Color.show b)
  | Wild -> "Wild"
