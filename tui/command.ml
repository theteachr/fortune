open Fortune

type play =
  | Self
  | AsMoney
  | WithColor of Color.t

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
            color |> Color.of_string |> Option.map (fun c -> WithColor c)
        | _ -> None
      in
      Some (Play (index, command))

let message = function
  | `Not_monetizable -> "You can't play that card as money."
  | `Invalid_color -> "You can't play that card with that color."
  | `Missing_color -> "You need to specify a color to play that card."
  | `Not_a_property -> "You can't play that card as a property."
  | `Plays_exhausted -> "You can't play more than 3 cards in a round."
  | `Invalid_index -> "Please enter a valid number."

let exec_play card game = function
  | Self -> Game.play card game
  | AsMoney -> Game.play_as_money card game
  | WithColor c -> Game.play_as_color card c game

let exec (ui : Ui.t) command =
  let ( >>= ) = Result.bind in
  let next =
    match command with
    | Play (n, as_) ->
        ui.game
        |> Game.current_player
        |> Player.use_card n
        |> Option.to_result ~none:`Invalid_index
        >>= fun (card, player) ->
        exec_play card (Game.set_current_player player ui.game) as_
    | End_round -> Ok (Game.next_round ui.game)
  in
  match next with
  | Ok game -> { ui with game }
  | Error e -> { ui with error_message = Some (message e) }
