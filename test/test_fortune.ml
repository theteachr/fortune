open Base
open Fortune
open Stdio
module Tui = Tui.Make (Tui.Color_ascii)

let%test_unit "106 cards in the shuffled deck" =
  [%test_eq: int] 106 Deck.(count default)

let game deck =
  [ "ocaml"; "reason"; "melange"; "dune" ]
  |> List.map ~f:Player.make
  |> Game.start deck

let render game = game |> Tui.show |> print_string
let shuffled_deck = Deck.(shuffle ~seed:10 default)
let default_game = game shuffled_deck

let%expect_test "default game" =
  render default_game;
  [%expect
    {|
    ocaml

    Hand -

    0. SKYBLUE BROWN
    1. M4
    2. RENT: BROWN SKYBLUE
    3. PASS GO
    4. JUST SAY NO

    Bank -



    Properties -



    86 card(s) left in the deck.
    |}]

let%expect_test "play simple property cards" =
  let game = game Deck.default in
  render game;
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



    86 card(s) left in the deck. |}];
  Game.(game |> play_card 0 |> play_card 2) |> render;
  [%expect {|
    ocaml

    Hand -

    0. BLUE
    1. BROWN
    2. GREEN

    Bank -



    Properties -

    0. BROWN
    1. BLUE

    86 card(s) left in the deck. |}]

let%expect_test "play a money card" =
  default_game |> Game.play_card 1 |> render;
  [%expect
    {|
    ocaml

    Hand -

    0. SKYBLUE BROWN
    1. RENT: BROWN SKYBLUE
    2. PASS GO
    3. JUST SAY NO

    Bank -

    0. M4

    Properties -



    86 card(s) left in the deck.
    |}]

let%expect_test "play an action card as money" =
  Game.(default_game |> play_money 4 |> play_money 3) |> render;
  [%expect
    {|
    ocaml

    Hand -

    0. SKYBLUE BROWN
    1. M4
    2. RENT: BROWN SKYBLUE

    Bank -

    0. M1 (PASS GO)
    1. M4 (JUST SAY NO)

    Properties -



    86 card(s) left in the deck.
    |}]
