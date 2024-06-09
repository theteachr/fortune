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
    | _ -> failwith "not enought players"
end

type t = {
  draw_pile: Deck.t;
  play_pile: Card.t list;
  players: Player.t Round.t;
}

let display { draw_pile; players; _ } =
  let player = Round.current players in
  Printf.printf {|
Monopoly Deal

%s is playing.

Hand -
%s

Properties -
%s

%d card(s) left in the deck.
|} player.name "HAND" "PROPERTIES" (Deck.count draw_pile)
