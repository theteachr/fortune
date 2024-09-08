open Base
open Fortune
open Stdio

let%test_unit "106 cards in the shuffled deck" =
  [%test_eq: int] 106 Deck.(count default)

let game ?(deck = Deck.default) () =
  [ "ocaml"; "reason"; "melange"; "dune" ]
  |> List.map ~f:Player.make
  |> Game.start deck

let shuffled_deck = Deck.(shuffle ~seed:10 default)

let%expect_test "remove_from_hand works" =
  let _, player = () |> game |> Game.current_player |> Player.use_card 0 in
  Tui.Player.show_hand player |> print_endline;
  [%expect {|
    0. BLUE
    1. BROWN
    2. BROWN
    3. GREEN |}]

let%expect_test "default game" =
  game () |> Game.play_card 0 |> Game.play_card 2 |> Tui.draw |> print_endline;
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

let%expect_test "playing a money card should work" =
  game ~deck:shuffled_deck () |> Game.play_card 1 |> Tui.draw |> print_endline;
  [%expect
    {|
    ==== MONOPOLY DEAL ====

    ocaml is playing.

    Hand -

    0. SKYBLUEBROWN
    1. BROWNSKYBLUE RENT
    2. PASS GO
    3. JUST SAY NO

    Bank -

    0. M4

    Properties -



    86 card(s) left in the deck. |}]
