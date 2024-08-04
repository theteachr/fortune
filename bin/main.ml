open Fortune

module Command = struct
  type t =
    | Play of int
    | Quit

  let parse line =
    match String.split_on_char ' ' line with
    | [ "p"; n ] -> int_of_string_opt n |> Option.map (fun n -> Play n)
    | [ "q" ] -> Some Quit
    | _ -> None
end

let clear_screen () = Sys.command "clear" |> ignore

let rec loop game =
  clear_screen ();
  print_endline @@ Tui.show_game game;
  print_string "> ";
  let command = read_line () |> Command.parse in
  match command with
  | Some Command.Quit -> ()
  | Some (Command.Play n) -> (
      let current_player = Game.current_player game in
      let card = Player.get n current_player in
      match card with
      | Card.Property (Simple color) ->
          let player =
            current_player
            |> Player.add_property (Property.use_simple color)
            |> Player.remove_from_hand n
          in
          game |> Game.update_current_player player |> loop
      | _ -> failwith "todo")
  | _ -> loop game

let () =
  [ "ocaml"; "reason"; "melange"; "dune" ]
  |> List.map Player.make
  |> Game.start
  |> loop
