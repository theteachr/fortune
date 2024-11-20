type t =
  | Wild
  | Dual of Dual.t

let value = function
  | Wild -> 3
  | Dual _ -> 1
