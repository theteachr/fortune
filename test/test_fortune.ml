open Base
open Fortune
open Stdio

let%test_unit "106 cards in the shuffled deck" =
  [%test_eq: int] 106 Deck.(count default)

let game deck =
  [ "ocaml"; "reason"; "melange"; "dune" ]
  |> List.map ~f:Player.make
  |> Game.start deck

let render_game game = game |> Tui.draw |> print_endline

let shuffled_deck = Deck.(shuffle ~seed:10 default)

let%expect_test "remove_from_hand works" =
  let game = game Deck.default in
  let player = Game.current_player game in
  Tui.Player.show_hand player.hand |> print_endline;
  [%expect {|
    0. BLUE
    1. BLUE
    2. BROWN
    3. BROWN
    4. GREEN |}];
  let _, player = Player.use_card 0 player in
  Tui.Player.show_hand player.hand |> print_endline;
  [%expect {|
    0. BLUE
    1. BROWN
    2. BROWN
    3. GREEN |}]

let%expect_test "default game" =
  let game = game Deck.default in
  render_game game;
  [%expect {|
    ocaml

    Hand -

    0. BLUE
    1. BLUE
    2. BROWN
    3. BROWN
    4. GREEN

    Bank -



    Properties -



    86 card(s) left in the deck.
    |}];
  let game = Game.play_card 0 game in
  render_game game;
  [%expect {|
    ocaml

    Hand -

    0. BLUE
    1. BROWN
    2. BROWN
    3. GREEN

    Bank -



    Properties -

    0. BLUE

    86 card(s) left in the deck.
    |}];
  game
  |> Game.play_card 2
  |> Tui.draw
  |> print_endline;
  [%expect
    {|
    ocaml

    Hand -

    0. BLUE
    1. BROWN
    2. GREEN

    Bank -



    Properties -

    0. BROWN
    1. BLUE

    86 card(s) left in the deck.
    |}]

let%expect_test "playing a money card should work" =
  shuffled_deck |> game |> Game.play_card 1 |> Tui.draw |> print_endline;
  [%expect
    {|
    ocaml

    Hand -

    0. SKYBLUE BROWN
    1. BROWNSKYBLUE RENT
    2. PASS GO
    3. JUST SAY NO

    Bank -

    0. M4

    Properties -



    86 card(s) left in the deck.
    |}]
