open Fortune

let read_command () =
  print_string "> ";
  let line = read_line () in
  if line = "q" then None else Some line

let () =
  let game =
    [ "ocaml"; "reason"; "melange"; "dune" ]
    |> List.map Player.make
    |> Game.start
  in
  read_command
  |> Seq.of_dispenser
  |> Seq.filter_map Command.parse
  |> Seq.scan Command.exec game
  |> Seq.take_while Game.is_not_over
  |> Seq.map Tui.draw
  |> Seq.iter (fun s ->
         Tui.clear_screen ();
         print_endline s)
