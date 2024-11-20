type building =
  | House
  | Hotel

type action =
  | DealBreaker
  | ForcedDeal
  | SlyDeal
  | JustSayNo
  | DebtCollector
  | Birthday
  | DoubleTheRent
  | PassGo

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
  | DealBreaker -> 5
  | ForcedDeal -> 3
  | SlyDeal -> 3
  | JustSayNo -> 4
  | DebtCollector -> 3
  | Birthday -> 2
  | DoubleTheRent -> 1
  | PassGo -> 1

let building_value = function
  | House -> 3
  | Hotel -> 4

let value = function
  | Action a -> action_value a
  | Building b -> building_value b
  | Rent r -> Rent.value r
