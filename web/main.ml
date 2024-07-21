let () =
  [ "theteachr"; "injuly"; "kaus"; "manas" ]
  |> List.map Player.make
  |> Fortune.Game.start
  |> Show.game
  |> print_endline
