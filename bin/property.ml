include Fortune.Property

let show = function
  | Simple color -> Color.show color
  | Dual (a, b) -> Printf.sprintf "%s%s" (Color.show a) (Color.show b)
  | Wild -> "Wild"
