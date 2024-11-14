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

let current_player { players; _ } = Round.current players

let set_current_player player game =
  { game with players = Round.set_current player game.players }

let add_played_card card game =
  { game with played_cards = card :: game.played_cards }

let is_not_over _ = true

let draw_two game =
  (* TODO: Handle the case where the deck does not have enough cards *)
  let cards, draw_pile = Deck.take 2 game.draw_pile in
  let player = List.fold_left Player.take (current_player game) cards in
  { (set_current_player player game) with draw_pile }

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
  draw_two { draw_pile; players; play_pile = []; played_cards = [] }

let next_round game =
  (* TODO: Ask the current player to discard if they have > 7 *)
  draw_two { game with players = Round.step game.players; played_cards = [] }

let use_player_card n game =
  if List.length game.played_cards < 3 then
    let player = current_player game in
    player |> Player.use_card n |> Option.to_result ~none:`Invalid_index
  else Error `Plays_exhausted

let ( let* ) = Result.bind

(* --- PLAY FUNCTIONS --- *)

let play_action action game =
  match action with
  | Action.PassGo ->
      game |> draw_two |> add_played_card (Action action) |> Result.ok
  | _ -> failwith "TODO"

let play n game =
  let* card, player = use_player_card n game in
  match card with
  | Card.Property card ->
      let* card = Property.use card in
      game
      |> set_current_player (Player.add_property card player)
      |> add_played_card (Property card)
      |> Result.ok
  | Card.Money value ->
      let card = Money.M value in
      game
      |> set_current_player (Player.add_money card player)
      |> add_played_card (Money card)
      |> Result.ok
  | Card.Action action -> play_action action (set_current_player player game)

let play_as_money n game =
  let* card, player = use_player_card n game in
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
  let* card, player = use_player_card n game in
  let* property =
    match card with
    | Card.Property card -> Property.use ~color card
    | _ -> Error `Not_a_property
  in
  game
  |> set_current_player (Player.add_property property player)
  |> add_played_card (Property property)
  |> Result.ok
