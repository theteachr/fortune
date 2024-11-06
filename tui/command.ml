type play =
  | Self
  | AsMoney
  | WithColor of Fortune.Color.t

(* XXX: Would we always need an index?
   Seems like so for now. *)
type t = int * play

let ( let* ) = Option.bind

let parse line =
  let* n, rest =
    match String.split_on_char ' ' line with
    | [] -> None
    | n :: rest -> Some (n, rest)
  in
  let* index = int_of_string_opt n in
  let* command =
    match rest with
    | [ "p" ] | [] -> Some Self
    | [ "m" ] -> Some AsMoney
    | [ "c"; color ] ->
        color |> Fortune.Color.of_string |> Option.map (fun c -> WithColor c)
    | _ -> None
  in
  Some (index, command)

let message = function
  | `Not_monetizable -> "You can't play that card as money."
  | `Invalid_color -> "You can't play that card with that color."
  | `Missing_color -> "You need to specify a color to play that card."
  | `Not_a_property -> "You can't play that card as a property."

let exec Ui.{ game; _ } (n, play) =
  let next =
    match play with
    | Self -> Fortune.Game.play n game
    | AsMoney -> Fortune.Game.play_as_money n game
    | WithColor c -> Fortune.Game.play_as_color n c game
  in
  match next with
  | Ok game -> Ui.{ game; error_message = None }
  | Error e -> Ui.{ game; error_message = Some (message e) }
