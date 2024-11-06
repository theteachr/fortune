type play =
  | Self
  | AsMoney
  | WithColor of string

type t = Play of int * play

let ( let* ) = Option.bind

let parse line =
  line |> String.split_on_char ' ' |> function
  | "p" :: n :: extra ->
      let* index = int_of_string_opt n in
      let play =
        match extra with
        | [] -> Self
        | [ "m" ] -> AsMoney
        | [ color ] -> WithColor color
        | _ -> failwith "todo"
      in
      Play (index, play) |> Option.some
  | _ -> None

let exec game = function
  | Play (n, Self) -> Fortune.Game.play_card n game
  | Play (n, AsMoney) -> Fortune.Game.play_money n game
  | _ -> game
