module Round = struct
  type 'a t = 'a * 'a * 'a list

  let current (player, _, _) = player
  let opponents (_, next, rest) = (next, rest)
  let set_current curr (_, next, rest) = (curr, next, rest)

  let step (curr, next, rest) =
    match rest with
    | [] -> (next, curr, [])
    | h :: t -> (next, h, t @ [ curr ])

  let of_list = function
    | a :: b :: rest -> (a, b, rest)
    | _ -> failwith "not enough players"
end

type t = {
  draw_pile: Deck.t;
  play_pile: Card.t list;
  players: Player.t Round.t;
}

let start players =
  Printf.printf "%d cards found in the deck.\n" Deck.(count default);
  (* Distribute 5 cards per player from the deck *)
  let distribute (deck, players) player =
    let cards, deck = Deck.take 5 deck in
    let player = List.fold_left Player.take player cards in
    (deck, player :: players)
  in
  let draw_pile, players =
    List.fold_left distribute (Deck.default, []) players
  in
  let players = players |> List.rev |> Round.of_list in
  { draw_pile; players; play_pile = [] }

let current_player { players; _ } = Round.current players

let update_current_player player game =
  { game with players = Round.set_current player game.players }
