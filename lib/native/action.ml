type rent =
  | Wild
  | Dual of Color.t * Color.t

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
  | Rent of rent
