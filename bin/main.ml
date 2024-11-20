open Fortune
module Ui = Tui.Ui.Make (Tui.Ui.Color_unicode)

let () =
  let deck = Deck.(shuffle default) in
  let game =
    [ "ocaml"; "reason"; "melange"; "dune" ]
    |> List.map Player.make
    |> Game.start deck
    |> Ui.init
  in
  Ui.read_input
  |> Seq.of_dispenser
  |> Seq.filter_map Tui.Command.parse
  |> Seq.scan Tui.Command.exec game
  |> Seq.take_while (Fun.negate Ui.game_over)
  |> Seq.iter Ui.render
