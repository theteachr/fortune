type t =
  | M of int
  | Action of Action.t

let value = function
  | M v -> v
  | Action a -> Action.value a
