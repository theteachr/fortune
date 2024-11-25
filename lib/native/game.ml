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
  play_pile: Action.Used.t list;
  players: Player.t Round.t;
  played_cards: Card.Used.t list;
}

let current_player { players; _ } = Round.current players

let set_current_player player game =
  { game with players = Round.set_current player game.players }

let add_played_card card game =
  { game with played_cards = card :: game.played_cards }

let is_over _ = false (* TODO *)

let draw ?(n = 2) game =
  let cards, draw_pile = Deck.take n game.draw_pile in
  (* If there aren't enough cards in the deck, take the remaining after
     shuffling the play pile, and make that the new draw pile. *)
  let remaining = n - List.length cards in
  let additional_cards, draw_pile, play_pile =
    if remaining > 0 then
      let deck =
        game.play_pile
        |> List.map Action.Used.reset
        |> List.mapi (fun id action -> Card.{ id; kind = Card.Action action })
        |> Deck.of_list
        |> Deck.shuffle
      in
      let cards, draw_pile = Deck.take remaining deck in
      (cards, draw_pile, [])
    else ([], draw_pile, game.play_pile)
  in
  let player =
    additional_cards
    |> List.rev_append cards
    |> List.fold_left Player.take (current_player game)
  in
  { (set_current_player player game) with draw_pile; play_pile }

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
  draw { draw_pile; players; play_pile = []; played_cards = [] }

let next_round game =
  (* TODO: Ask the current player to discard if they have > 7 *)
  draw { game with players = Round.step game.players; played_cards = [] }

let ( let* ) = Result.bind

(* --- PLAY FUNCTIONS --- *)

let put_center card game = { game with play_pile = card :: game.play_pile }

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

let play_action_reg game = function
  | Action.Pass_go -> draw game
  | _ -> failwith "TODO"

let play_action action game =
  match action with
  | Action.(Action action) ->
      let used = Action.Used.Regular action in
      action
      |> play_action_reg game
      |> add_played_card @@ Action used
      |> put_center used
      |> Result.ok
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
