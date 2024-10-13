type t =
  | Money of int
  | Property of Property.card
  | Action of Action.t

let money value = Money value
let simple_property color = Property (Simple color)
let dual_property colors = Property (Dual colors)
let action a = Action a
let rent (a, b) = Action (Rent (Dual (a, b)))
let wild_property = Property Property.wild
let wild_rent = Action (Rent Wild)
