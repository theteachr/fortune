let () =
  [ "theteachr"; "injuly"; "kaus" ]
  |> List.map Player.create
  |> Fortune.Game.start
  |> Show.game
  |> print_endline
