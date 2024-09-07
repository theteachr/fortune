type t =
  | Money of Money.t
  | Property of Property.card
  | Action of Action.t

let money value = Money (M value)
let simple_property color = Property (Simple color)
let dual_property colors = Property (Dual colors)
let action a = Action a
let rent (a, b) = Action (Rent (Dual (a, b)))
let wild_property = Property Wild
let wild_rent = Action (Rent Wild)
