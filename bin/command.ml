type t =
  | Play of int
  | Quit
  | Bad

let play n = Play n

let parse line =
  match String.split_on_char ' ' line with
  | [ "p"; n ] -> int_of_string_opt n |> Option.fold ~none:Bad ~some:play
  | [ "q" ] -> Quit
  | _ -> Bad

let exec_play n game =
  let card = game |> Game.current_player |> Player.get n in
  match card with
  | Card.Property (Simple color) ->
      let player =
        game
        |> Game.current_player
        |> Player.add_property (Property.use_simple color)
        |> Player.remove_from_hand n
      in
      game |> Game.set_current_player player |> Option.some
  | _ -> Some game

let exec (game : Game.t) = function
  | Play n -> exec_play n game
  | Bad -> Some game
  | Quit -> None
