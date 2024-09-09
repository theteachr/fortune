open Fortune

let () =
  let deck = Deck.(shuffle default) in
  let game =
    [ "ocaml"; "reason"; "melange"; "dune" ]
    |> List.map Player.make
    |> Game.start deck
  in
  let open Seq in
  Tui.read_input
  |> of_dispenser
  |> filter_map Tui.parse_input
  |> scan Tui.update game
  |> take_while Game.is_not_over
  |> iter Tui.render
