include Money

let show = function
  | M value -> Printf.sprintf "%d M" value
  | Action action -> Printf.sprintf "%s" (Action.show action)
