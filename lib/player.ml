type t = {
  name: string;
  bank: Money.t list;
  properties: Property.t list;
  hand: Card.t list;
}

let draw ?(n = 1) deck player =
  let cards, deck = Deck.take n deck in
  ({ player with hand = player.hand @ cards }, deck)

let add_property property player =
  { player with properties = property :: player.properties }

let add_money money player = { player with bank = money :: player.bank }
let create name = { name; bank = []; properties = []; hand = [] }
