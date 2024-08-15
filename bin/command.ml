type t =
  | Play of int
  | Quit
  | Bad

let play n = Play n

let parse line =
  line |> String.split_on_char ' ' |> function
  | [ "p"; n ] -> n |> int_of_string_opt |> Option.fold ~none:Bad ~some:play
  | [ "q" ] -> Quit
  | _ -> Bad

let exec game = function
  | Play n -> game |> Game.current_player |> Player.get n |> Game.play_card game
  | Bad | Quit -> game
