open Fortune

let () =
  [ "ocaml"; "reason"; "melange"; "dune" ]
  |> List.map Player.make
  |> Fortune.Game.start
  |> Tui.show_game
  |> print_endline
