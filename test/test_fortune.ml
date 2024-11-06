open Base
open Fortune
open Stdio
module Tui_ = Tui
module Tui = Tui.Ui.Make (Tui.Ui.Color_ascii)

let%test_unit "106 cards in the shuffled deck" =
  [%test_eq: int] 106 Deck.(count default)

let game deck =
  [ "ocaml"; "reason"; "melange"; "dune" ]
  |> List.map ~f:Player.make
  |> Game.start deck
  |> Tui.init

let render game = game |> Tui.show |> print_endline
let exec = Fn.flip Tui_.Command.exec
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

    []
    |}]

let%expect_test "play simple property cards" =
  let game = game Deck.default in
  render game;
  [%expect
    {|
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

    []
    |}];
  (* Game.(game |> play_card 0 >>= play_card 2) |> Result.iter ~f:render;*)
  game |> exec (Play (0, Self)) |> exec (Play (2, Self)) |> render;
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

    []
    |}]

let%expect_test "play a money card" =
  default_game |> exec (Play (1, Self)) |> render;
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

    []
    |}]

let%expect_test "play an action card as money" =
  default_game |> exec (Play (4, AsMoney)) |> exec (Play (3, AsMoney)) |> render;
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

    []
    |}]

let%expect_test "display error on trying to play property card as money" =
  default_game |> exec (Play (0, AsMoney)) |> render;
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

    [You can't play that card as money.]
    |}]
