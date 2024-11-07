type simple = Color.t
type dual = Dual.t
type wild = unit

type card =
  | Simple of simple
  | Dual of dual
  | Wild of wild

type t =
  | Simple of Color.t
  | Dual of Dual.t * Dual.choice
  | Wild of Color.t

let color (card : t) =
  match card with
  | Simple color -> color
  | Dual (colors, choice) -> Dual.color (colors, choice)
  | Wild color -> color

let use_simple color = Simple color
let use_dual colors choice = Dual (colors, choice)
let use_wild _ color = Wild color
let wild : card = Wild ()
let ( let* ) = Result.bind

let use ?color (card : card) =
  match card with
  | Simple color -> Ok (use_simple color)
  | Dual dual ->
      let* color = Option.to_result ~none:`Missing_color color in
      let* choice =
        match dual with
        | l, _ when l = color -> Ok Dual.L
        | _, r when r = color -> Ok Dual.R
        | _ -> Error `Invalid_color
      in
      Ok (use_dual dual choice)
  | Wild w ->
      color |> Option.to_result ~none:`Missing_color |> Result.map (use_wild w)
