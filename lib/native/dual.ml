type t = Color.t * Color.t

type choice =
  | L
  | R

type active = t * choice

let color = function
  | (a, _), L -> a
  | (_, b), R -> b
