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
let exec_play (index, play) game = Tui_.Command.exec game (Play (index, play))
let shuffled_deck = Deck.(shuffle ~seed:10 default)

let deck_with_very_wild_card =
  let _, deck = Deck.take 1 shuffled_deck in
  Deck.add (Card.Property Property.wild) deck

let default_game = game shuffled_deck

let%expect_test "default game" =
  render default_game;
  [%expect
    {|
    ocaml

    Hand -

    0. BLUE
    1. PASS GO
    2. SKYBLUE BROWN
    3. M4
    4. RENT: BROWN SKYBLUE
    5. PASS GO
    6. JUST SAY NO

    Bank -



    Properties -



    This turn -



    84 card(s) left in the deck.

    []
    |}]

let%expect_test "play simple property cards" =
  let game = game Deck.default in
  render game;
  [%expect
    {|
    ocaml

    Hand -

    0. RED
    1. RED
    2. BLUE
    3. BLUE
    4. BROWN
    5. BROWN
    6. GREEN

    Bank -



    Properties -



    This turn -



    84 card(s) left in the deck.

    []
    |}];
  game |> exec_play (0, Self) |> exec_play (2, Self) |> render;
  [%expect
    {|
    ocaml

    Hand -

    0. RED
    1. BLUE
    2. BROWN
    3. BROWN
    4. GREEN

    Bank -



    Properties -

    0. BLUE
    1. RED

    This turn -

    BLUE
    RED

    84 card(s) left in the deck.

    []
    |}]

let%expect_test "play a money card" =
  default_game |> exec_play (3, Self) |> render;
  [%expect
    {|
    ocaml

    Hand -

    0. BLUE
    1. PASS GO
    2. SKYBLUE BROWN
    3. RENT: BROWN SKYBLUE
    4. PASS GO
    5. JUST SAY NO

    Bank -

    0. M4

    Properties -



    This turn -

    M4

    84 card(s) left in the deck.

    []
    |}]

let%expect_test "play action cards as money" =
  default_game |> exec_play (6, AsMoney) |> exec_play (5, AsMoney) |> render;
  [%expect
    {|
    ocaml

    Hand -

    0. BLUE
    1. PASS GO
    2. SKYBLUE BROWN
    3. M4
    4. RENT: BROWN SKYBLUE

    Bank -

    0. M1 (PASS GO)
    1. M4 (JUST SAY NO)

    Properties -



    This turn -

    M1 (PASS GO)
    M4 (JUST SAY NO)

    84 card(s) left in the deck.

    []
    |}]

let%expect_test "display error on trying to play property card as money" =
  default_game |> exec_play (0, AsMoney) |> render;
  [%expect
    {|
    ocaml

    Hand -

    0. BLUE
    1. PASS GO
    2. SKYBLUE BROWN
    3. M4
    4. RENT: BROWN SKYBLUE
    5. PASS GO
    6. JUST SAY NO

    Bank -



    Properties -



    This turn -



    84 card(s) left in the deck.

    [You can't play that card as money.]
    |}]

let%expect_test "play wild property - first color" =
  default_game |> exec_play (2, WithColor SkyBlue) |> render;
  [%expect
    {|
    ocaml

    Hand -

    0. BLUE
    1. PASS GO
    2. M4
    3. RENT: BROWN SKYBLUE
    4. PASS GO
    5. JUST SAY NO

    Bank -



    Properties -

    0. [SKYBLUE] BROWN

    This turn -

    [SKYBLUE] BROWN

    84 card(s) left in the deck.

    []
    |}]

let%expect_test "play wild property - second color" =
  default_game |> exec_play (2, WithColor Brown) |> render;
  [%expect
    {|
    ocaml

    Hand -

    0. BLUE
    1. PASS GO
    2. M4
    3. RENT: BROWN SKYBLUE
    4. PASS GO
    5. JUST SAY NO

    Bank -



    Properties -

    0. SKYBLUE [BROWN]

    This turn -

    SKYBLUE [BROWN]

    84 card(s) left in the deck.

    []
    |}]

let%expect_test "play wild property with a wrong color" =
  default_game |> exec_play (2, WithColor Black) |> render;
  [%expect
    {|
    ocaml

    Hand -

    0. BLUE
    1. PASS GO
    2. SKYBLUE BROWN
    3. M4
    4. RENT: BROWN SKYBLUE
    5. PASS GO
    6. JUST SAY NO

    Bank -



    Properties -



    This turn -



    84 card(s) left in the deck.

    [You can't play that card with that color.]
    |}]

let%expect_test "play very wild card" =
  let game = game deck_with_very_wild_card in
  render game;
  [%expect
    {|
    ocaml

    Hand -

    0. BLUE
    1. PASS GO
    2. WILD PROPERTY
    3. M4
    4. RENT: BROWN SKYBLUE
    5. PASS GO
    6. JUST SAY NO

    Bank -



    Properties -



    This turn -



    84 card(s) left in the deck.

    []
    |}];
  game |> exec_play (2, WithColor Black) |> render;
  [%expect
    {|
    ocaml

    Hand -

    0. BLUE
    1. PASS GO
    2. M4
    3. RENT: BROWN SKYBLUE
    4. PASS GO
    5. JUST SAY NO

    Bank -



    Properties -

    0. BLACK [WILD]

    This turn -

    BLACK [WILD]

    84 card(s) left in the deck.

    []
    |}]

let%expect_test "disallow playing more than 3 cards" =
  default_game
  |> exec_play (2, WithColor SkyBlue)
  |> exec_play (2, Self)
  |> exec_play (3, AsMoney)
  |> exec_play (3, AsMoney)
  |> render;
  [%expect
    {|
      ocaml

      Hand -

      0. BLUE
      1. PASS GO
      2. RENT: BROWN SKYBLUE
      3. JUST SAY NO

      Bank -

      0. M1 (PASS GO)
      1. M4

      Properties -

      0. [SKYBLUE] BROWN

      This turn -

      M1 (PASS GO)
      M4
      [SKYBLUE] BROWN

      84 card(s) left in the deck.

      [You can't play more than 3 cards in a round.]
      |}]
