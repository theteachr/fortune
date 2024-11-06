type building =
  | House
  | Hotel

type t =
  | DealBreaker
  | ForcedDeal
  | SlyDeal
  | JustSayNo
  | DebtCollector
  | Birthday
  | DoubleTheRent
  | Building of building
  | PassGo
  | Rent of Rent.t

let value = function
  | DealBreaker -> 5
  | ForcedDeal -> 3
  | SlyDeal -> 3
  | JustSayNo -> 4
  | DebtCollector -> 3
  | Birthday -> 2
  | DoubleTheRent -> 1
  | Building House -> 3
  | Building Hotel -> 4
  | PassGo -> 1
  | Rent rent -> Rent.value rent
