open Fortune
module Tui = Tui.Make (Tui.Color_unicode)

let () =
  let deck = Deck.(shuffle default) in
  let game =
    [ "ocaml"; "reason"; "melange"; "dune" ]
    |> List.map Player.make
    |> Game.start deck
  in
  Tui.read_input
  |> Seq.of_dispenser
  |> Seq.filter_map Tui.parse_input
  |> Seq.scan Tui.update game
  |> Seq.take_while Game.is_not_over
  |> Seq.iter Tui.render
