type t =
  | Black
  | Blue
  | Brown
  | Green
  | Magenta
  | Orange
  | Red
  | Sky_blue
  | Turquoise
  | Yellow

let rgb = function
  | Black -> (0, 0, 0)
  | Blue -> (10, 147, 150)
  | Brown -> (150, 75, 0)
  | Green -> (83, 221, 108)
  | Magenta -> (214, 122, 177)
  | Orange -> (255, 120, 79)
  | Red -> (232, 49, 81)
  | Sky_blue -> (132, 218, 235)
  | Turquoise -> (148, 210, 189)
  | Yellow -> (244, 233, 0)

let show color =
  let r, g, b = rgb color in
  Printf.sprintf "\x1b[38;2;%d;%d;%dm●\x1b[0m" r g b
