open Fortune

let () =
  [ "ocaml"; "reason"; "melange"; "dune" ]
  |> List.map Player.make
  |> Fortune.Game.start
  |> Show.game
  |> print_endline
