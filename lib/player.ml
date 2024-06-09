type t = {
  name: string;
  bank: Money.t list;
  properties: Property.t list;
  hand: Card.t list;
}

let play_property property player =
  { player with properties = property :: player.properties }

let play_money money player =
  { player with bank = money :: player.bank }

let display_hand { hand; _ } =
  hand |> List.map Card.show |> String.concat "\n"

let create name = { name; bank = []; properties = []; hand = [] } 
