module Round = struct
  type 'a t = 'a * 'a * 'a list

  let current (a, _, _) = a
  let set_current curr (_, b, rest) = (curr, b, rest)

  let of_list = function
    | a :: b :: rest -> Some (a, b, rest)
    | _ -> None
end

type t = { players: Player.t Round.t }

let ( let* ) = Option.bind

let start players =
  (* Distribute 5 cards per player from the deck *)
  let _, players =
    let distribute (deck, players) player =
      let cards, deck = Deck.take 5 deck in
      let player = List.fold_left Player.take player cards in
      (deck, player :: players)
    in
    List.fold_left distribute (Deck.default, []) players
  in
  let* players = players |> List.rev |> Round.of_list in
  Some { players }

let current_player { players; _ } = Round.current players

let play_card ({ players } as game) card =
  match card with
  | Card.Property (Simple color) ->
      let player =
        players
        |> Round.current
        |> Player.add_property (Property.use_simple color)
        |> Player.remove_from_hand card
      in
      { players = Round.set_current player players }
  | _ -> game
