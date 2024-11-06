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
  | "blue" -> Some Blue
  | "br" -> Some Brown
  | "g" -> Some Green
  | "mag" -> Some Magenta
  | "o" -> Some Orange
  | "r" -> Some Red
  | "sky" -> Some SkyBlue
  | "tur" -> Some Turquoise
  | "y" -> Some Yellow
  | _ -> None
