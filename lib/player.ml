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
