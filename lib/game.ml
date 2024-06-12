module Round = struct
  type 'a t = 'a * 'a * 'a list

  let current (player, _, _) = player
  let opponents (_, next, rest) = (next, rest)

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
  let draw_pile, players =
    players
    |> List.fold_left
         (fun (deck, players) player ->
           let player, deck = Player.draw ~n:5 deck player in
           (deck, player :: players))
         (Deck.default, [])
  in
  let players = players |> List.rev |> Round.of_list in
  { draw_pile; players; play_pile = [] }
