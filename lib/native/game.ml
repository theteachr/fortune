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
  { draw_pile; players; play_pile = [] }

let current_player { players; _ } = Round.current players

let set_current_player player game =
  { game with players = Round.set_current player game.players }

let is_not_over _ = true

let play n game =
  let card, player = Player.use_card n (current_player game) in
  match card with
  | Card.Property (Simple color) ->
      let property = Property.use_simple color in
      let player = Player.add_property property player in
      Ok (set_current_player player game)
  | Card.Money value ->
      let player = Player.add_money (M value) player in
      Ok (set_current_player player game)
  | _ -> Ok game

let play_as_money n game =
  let card, player = Player.use_card n (current_player game) in
  match card with
  | Card.Money value ->
      let player = Player.add_money (M value) player in
      Ok (set_current_player player game)
  | Card.Action action ->
      let player = Player.add_money (Action action) player in
      Ok (set_current_player player game)
  | _ -> Error `Not_monetizable

let play_as_color n color game =
  let card, player = Player.use_card n (current_player game) in
  match card with
  | Card.Property (Simple c) when c = color ->
      let property = Property.use_simple color in
      let player = Player.add_property property player in
      Ok (set_current_player player game)
  | Card.Property (Dual dual) ->
      let choice =
        match dual with
        | l, _ when l = color -> Ok Dual.L
        | _, r when r = color -> Ok Dual.R
        | _ -> Error `Invalid_color
      in
      choice
      |> Result.map (fun choice ->
             let property = Property.use_dual dual choice in
             let player = Player.add_property property player in
             set_current_player player game)
  | Card.Property (Wild w) ->
      let property = Property.use_wild w color in
      let player = Player.add_property property player in
      Ok (set_current_player player game)
  | _ -> Error `Invalid_color
