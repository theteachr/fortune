type t = Color.t * Color.t

type choice =
  | L
  | R

let color = function
  | (a, _), L -> a
  | (_, b), R -> b
