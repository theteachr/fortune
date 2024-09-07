open Base
open Fortune

let%test_unit "106 cards in the shuffled deck" =
  [%test_eq: int] 106 Deck.(count default)
