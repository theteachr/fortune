open Fortune

let clear_screen () = Sys.command "clear" |> ignore

let init game = ref game

let loop game_ref () =
  clear_screen ();
  Tui.draw !game_ref;
  print_string "> ";
  ()
  |> read_line
  |> Command.parse
  |> Command.exec !game_ref
  |> Option.map (fun game' -> game_ref := game'; game')

let () =
  [ "ocaml"; "reason"; "melange"; "dune" ]
  |> List.map Player.make
  |> Game.start
  |> init
  |> loop
  |> Seq.of_dispenser
  |> Seq.iter Tui.draw
