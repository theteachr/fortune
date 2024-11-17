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

let ( let* ) = Result.bind

(* --- PLAY FUNCTIONS --- *)

let play_property ?ctx (property : Property.card) game =
  let* property =
    match (ctx, property) with
    | Some (`Color c), Simple color when color != c -> Error `Invalid_color
    | Some `Money, Simple _ -> Error `Not_monetizable
    | _, Property.Simple color -> Ok (Property.use_simple color)
    | Some (`Choice c), Property.Dual dual -> Ok (Property.use_dual dual c)
    | Some (`Color c), Property.Wild w -> Ok (Property.use_wild w c)
    | None, _ -> Error `Missing_color
    | _ -> Error `Invalid_ctx
  in
  let player = game |> current_player |> Player.add_property property in
  game
  |> set_current_player player
  |> add_played_card (Property property)
  |> Result.ok

let play_action action game =
  match action with
  | Action.PassGo ->
      game |> draw_two |> add_played_card (Action action) |> Result.ok
  | _ -> failwith "TODO"

let play_money money game =
  game
  |> set_current_player (game |> current_player |> Player.add_money money)
  |> add_played_card (Money money)

let play ?ctx card game =
  (* XXX: Check if the player has exhausted their plays. *)
  let* _ =
    if List.length game.played_cards < 3 then Ok () else Error `Plays_exhausted
  in
  match card with
  | Card.Property card -> play_property ?ctx card game
  | Card.Money value -> Ok (play_money (Money.M value) game)
  | Card.Action action when ctx = Some `Money ->
      Ok (play_money (Money.Action action) game)
  | Card.Action action -> play_action action game
