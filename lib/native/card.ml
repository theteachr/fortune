type k =
  | Money of int
  | Property of Property.card
  | Action of Action.t

type t = {
  id: int;
  kind: k;
}

module Used = struct
  type t =
    | Money of Money.t
    | Property of Property.t
    | Action of Action.Used.t
end

let make_money value = Money value
let make_simple_property color = Property (Simple color)
let make_dual_property colors = Property (Dual colors)
let make_action a = Action (Action a)
let make_building b = Action (Building b)
let make_rent (a, b) = Action (Rent (Dual (a, b)))
let make_wild_property = Property Property.wild
let make_wild_rent = Action (Rent Wild)
