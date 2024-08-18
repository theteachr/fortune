type t = Play of int

let play n = Play n

let parse line =
  line |> String.split_on_char ' ' |> function
  | [ "p"; n ] -> n |> int_of_string_opt |> Option.map play
  | _ -> None

let exec_play n game =
  let card, player = game |> Game.current_player |> Player.use_card n in
  match card with
  | Card.Property (Simple color) ->
      let property = Property.use_simple color in
      let player = Player.add_property property player in
      Game.set_current_player player game
  | _ -> game

let exec game = function
  | Play n -> exec_play n game
