type t = Play of int

let play n = Play n

let parse line =
  line |> String.split_on_char ' ' |> function
  | [ "p"; n ] -> n |> int_of_string_opt |> Option.map play
  | _ -> None

let exec game = function
  | Play n -> Fortune.Game.play_card n game
