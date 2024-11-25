type building =
  | House
  | Hotel

type action =
  | Deal_breaker
  | Forced_deal
  | Sly_deal
  | Just_say_no
  | Debt_collector
  | Birthday
  | Double_the_rent
  | Pass_go

type t =
  | Action of action
  | Building of building
  | Rent of Rent.t

module Used = struct
  type rent =
    | Wild of Color.t
    | Dual of Dual.active

  type t =
    | Regular of action
    | Rent of rent

  let reset = function
    | Regular a -> Action a
    | Rent (Wild _) -> Rent Wild
    | Rent (Dual (colors, _)) -> Rent (Dual colors)
end

let action_value = function
  | Deal_breaker -> 5
  | Forced_deal -> 3
  | Sly_deal -> 3
  | Just_say_no -> 4
  | Debt_collector -> 3
  | Birthday -> 2
  | Double_the_rent -> 1
  | Pass_go -> 1

let building_value = function
  | House -> 3
  | Hotel -> 4

let value = function
  | Action a -> action_value a
  | Building b -> building_value b
  | Rent r -> Rent.value r
