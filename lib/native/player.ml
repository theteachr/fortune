type t = {
  name: string;
  bank: Money.t list;
  properties: Property.t list;
  hand: Card.t list;
}

let make name = { name; bank = []; properties = []; hand = [] }
let take player card = { player with hand = card :: player.hand }

let add_property property player =
  { player with properties = property :: player.properties }

let add_money money player = { player with bank = money :: player.bank }
let get n { hand; _ } = List.nth hand n

let remove_from_hand card player =
  let hand = List.filter (( != ) card) player.hand in
  { player with hand }

let use_card n player =
  let card = get n player in
  (card, remove_from_hand card player)
