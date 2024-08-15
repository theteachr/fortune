open Fortune

let () =
  let game =
    [ "ocaml"; "reason"; "melange"; "dune" ]
    |> List.map Player.make
    |> Game.start
  in
  Io.read_command
  |> Seq.of_dispenser
  |> Seq.scan Command.exec game
  |> Seq.iter Tui.draw
