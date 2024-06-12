type t = Card.t list

let count = List.length

let default =
  Card.
    [
      Money (Money.M 3);
      Property (Property.Simple Color.Blue);
      Money (Money.M 10);
      Property (Property.Simple Color.Green);
      Action Action.DealBreaker;
    ]

let take n deck =
  let rec take' n cards = function
    | deck when n = 0 -> (cards, deck)
    | [] -> (cards, [])
    | card :: rest -> take' (n - 1) (card :: cards) rest
  in
  take' n [] deck
