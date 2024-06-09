type t = Card.t list

let count = List.length

let default =
  Card.
    [
      Money (Money.M 3);
      Money (Money.M 10);
    ]
