type t =
  | Black
  | Blue
  | Brown
  | Green
  | Magenta
  | Orange
  | Red
  | SkyBlue
  | Turquoise
  | Yellow

let of_string = function
  | "k" | "black" -> Some Black
  | "bl" | "blue" -> Some Blue
  | "br" | "brown" -> Some Brown
  | "g" | "green" -> Some Green
  | "mag" | "magenta" | "pink" -> Some Magenta
  | "o" | "orange" -> Some Orange
  | "r" | "red" -> Some Red
  | "sky" | "lbl" | "lblue" -> Some SkyBlue
  | "tur" | "cyan" -> Some Turquoise
  | "y" | "yellow" -> Some Yellow
  | _ -> None
