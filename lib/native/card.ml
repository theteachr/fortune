type t =
  | Money of int
  | Property of Property.card
  | Action of Action.t

module Used = struct
  type t =
    | Money of Money.t
    | Property of Property.t
    (* TODO: Add additional state for action cards.
       Below are a few states that will be important.
       - Color chosen for a rent card
       - Amount of rent
       - The property a hotel card was used on *)
    | Action of Action.t
end

let money value = Money value
let simple_property color = Property (Simple color)
let dual_property colors = Property (Dual colors)
let action a = Action a
let rent (a, b) = Action (Rent (Dual (a, b)))
let wild_property = Property Property.wild
let wild_rent = Action (Rent Wild)
