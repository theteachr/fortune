module Hand = Map.Make (Int)

type t = {
  name: string;
  bank: Money.t list;
  properties: Property.t list;
  hand: Card.k Hand.t;
}

let make name = { name; bank = []; properties = []; hand = Hand.empty }

let take player Card.{ id; kind } =
  { player with hand = Hand.add id kind player.hand }

let take_many player cards = List.fold_left take player cards

let add_property property player =
  { player with properties = property :: player.properties }

let add_money money player = { player with bank = money :: player.bank }
let get n { hand; _ } = Hand.find_opt n hand
let remove_from_hand n player = { player with hand = Hand.remove n player.hand }

let use_card n player =
  player |> get n |> Option.map (fun card -> (card, remove_from_hand n player))
