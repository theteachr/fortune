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
  played_cards: Card.Used.t list;
}

let start deck players =
  (* Distribute 5 cards per player from the deck *)
  let draw_pile, players =
    let distribute (deck, players) player =
      let cards, deck = Deck.take 5 deck in
      let player = List.fold_left Player.take player cards in
      (deck, player :: players)
    in
    List.fold_left distribute (deck, []) players
  in
  let players = players |> List.rev |> Round.of_list in
  { draw_pile; players; play_pile = []; played_cards = [] }

let current_player { players; _ } = Round.current players

let set_current_player player game =
  { game with players = Round.set_current player game.players }

let add_played_card card game =
  { game with played_cards = card :: game.played_cards }

let is_not_over _ = true

let next_round game =
  (* TODO: Ask the current player to discard if they have > 7 *)
  (* TODO: Add two cards to the player's hand *)
  { game with players = Round.step game.players (* TODO: Reset played cards *) }

(* --- play functions --- *)
let ( let* ) = Result.bind

let player_moves_over game =
  if List.length game.played_cards = 3 then Error `Moves_over else Ok game

let play n game =
  let* game = player_moves_over game in
  let card, player = Player.use_card n (current_player game) in
  let* card, player, game =
    match card with
    | Card.Property card ->
        let* card = Property.use card in
        Ok (Card.Used.Property card, Player.add_property card player, game)
    | Card.Money value ->
        let card = Money.M value in
        Ok (Money card, Player.add_money card player, game)
    | _ -> failwith "TODO"
  in
  game |> set_current_player player |> add_played_card card |> Result.ok

let play_as_money n game =
  let* game = player_moves_over game in
  let card, player = Player.use_card n (current_player game) in
  let* money =
    match card with
    | Card.Money value -> Ok (Money.M value)
    | Card.Action action -> Ok (Action action)
    | _ -> Error `Not_monetizable
  in
  game
  |> set_current_player (Player.add_money money player)
  |> add_played_card (Money money)
  |> Result.ok

let play_as_color n color game =
  let* game = player_moves_over game in
  let card, player = Player.use_card n (current_player game) in
  let* property =
    match card with
    | Card.Property card -> Property.use ~color card
    | _ -> Error `Not_a_property
  in
  game
  |> set_current_player (Player.add_property property player)
  |> add_played_card (Property property)
  |> Result.ok
