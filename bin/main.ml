open Fortune

let clear_screen () = Sys.command "clear" |> ignore

let rec loop game =
  clear_screen ();
  game |> Tui.show_game |> print_endline;
  print_string "> ";
  read_line ()
  |> Command.parse
  |> Command.exec game
  |> Option.map (fun game' -> (game, game'))

let () =
  [ "ocaml"; "reason"; "melange"; "dune" ]
  |> List.map Player.make
  |> Game.start
  |> Seq.unfold loop
  |> Seq.iter (fun game -> game |> Tui.show_game |> print_endline)
