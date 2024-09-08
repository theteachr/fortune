open Base
open Fortune

let%test_unit "106 cards in the shuffled deck" =
  [%test_eq: int] 106 Deck.(count default)

let default_game =
  [ "ocaml"; "reason"; "melange"; "dune" ]
  |> List.map ~f:Player.make
  |> Game.start

let%expect_test "remove_from_hand works" =
  let _, player = Game.current_player default_game |> Player.use_card 0 in
  Tui.Player.show_hand player |> Stdio.print_endline;
  [%expect {|
    0. BLUE
    1. BROWN
    2. BROWN
    3. GREEN |}]

let%expect_test "default game" =
  default_game
  |> Game.play_card 0
  |> Game.play_card 2
  |> Tui.draw
  |> Stdio.print_endline;
  [%expect
    {|
    ==== MONOPOLY DEAL ====

    ocaml is playing.

    Hand -

    0. BLUE
    1. BROWN
    2. GREEN

    Bank -



    Properties -

    0. BROWN
    1. BLUE

    86 card(s) left in the deck. |}]
