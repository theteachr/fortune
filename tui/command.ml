type play =
  | Self
  | AsMoney
  | WithColor of Fortune.Color.t

type t =
  | Play of int * play
  | End_round

let ( let* ) = Option.bind

let parse line =
  match String.split_on_char ' ' line with
  | [] -> None
  | [ "e" ] -> Some End_round
  | n :: rest ->
      let* index = int_of_string_opt n in
      let* command =
        match rest with
        | [ "p" ] | [] -> Some Self
        | [ "m" ] -> Some AsMoney
        | [ color ] ->
            color
            |> Fortune.Color.of_string
            |> Option.map (fun c -> WithColor c)
        | _ -> None
      in
      Some (Play (index, command))

let message = function
  | `Not_monetizable -> "You can't play that card as money."
  | `Invalid_color -> "You can't play that card with that color."
  | `Missing_color -> "You need to specify a color to play that card."
  | `Not_a_property -> "You can't play that card as a property."
  | `Moves_over -> "You can't play more than 3 cards in a round."
  | `Invalid_index max_index ->
      Printf.sprintf "Please enter a number in range [0, %d]." max_index

let exec_play n game = function
  | Self -> Fortune.Game.play n game
  | AsMoney -> Fortune.Game.play_as_money n game
  | WithColor c -> Fortune.Game.play_as_color n c game

let exec Ui.{ game; _ } command =
  let next =
    match command with
    | Play (n, as_) -> exec_play n game as_
    | End_round -> Ok (Fortune.Game.next_round game)
  in
  match next with
  | Ok game -> Ui.{ game; error_message = None }
  | Error e -> Ui.{ game; error_message = Some (message e) }
