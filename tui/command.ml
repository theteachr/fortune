type play =
  | Self
  | AsMoney
  | WithColor of Fortune.Color.t

type t = Play of int * play

let ( let* ) = Option.bind

let parse line =
  line |> String.split_on_char ' ' |> function
  | "p" :: n :: extra ->
      let* index = int_of_string_opt n in
      let* play =
        match extra with
        | [] -> Some Self
        | [ "m" ] -> Some AsMoney
        | [ color ] ->
            color
            |> Fortune.Color.of_string
            |> Option.map (fun c -> WithColor c)
        | _ -> None
      in
      Play (index, play) |> Option.some
  | _ -> None

let message = function
  | `Not_monetizable -> "You can't play that card as money."
  | `Invalid_color -> "You can't play that card with that color."

let exec Ui.{ game; _ } command =
  let next =
    match command with
    | Play (n, Self) -> Fortune.Game.play_card n game
    | Play (n, AsMoney) -> Fortune.Game.play_money n game
    | Play (n, WithColor c) -> Fortune.Game.play_as_color n c game
  in
  match next with
  | Ok game -> Ui.{ game; error_message = None }
  | Error e -> Ui.{ game; error_message = Some (message e) }
