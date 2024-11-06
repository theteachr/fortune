type t =
  | Wild
  | Dual of Color.t * Color.t

let value = function
  | Wild -> 3
  | Dual _ -> 1
