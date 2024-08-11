open Fortune

let read_command () =
  print_string "> ";
  () |> read_line |> Command.parse |> function
  | Quit -> None
  | command -> Some command

let () =
  let game =
    [ "ocaml"; "reason"; "melange"; "dune" ]
    |> List.map Player.make
    |> Game.start
  in
  read_command
  |> Seq.of_dispenser
  |> Seq.scan Command.exec game
  |> Seq.iter Tui.draw
