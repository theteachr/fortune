include Fortune.Action

let show = function
  | DealBreaker -> "DealBreaker"
  | ForcedDeal -> "ForcedDeal"
  | SlyDeal -> "SlyDeal"
  | JustSayNo -> "JustSayNo"
  | DebtCollector -> "DebtCollector"
  | Birthday -> "Birthday"
  | DoubleTheRent -> "DoubleTheRent"
  | Building House -> "House"
  | Building Hotel -> "Hotel"
  | PassGo -> "PassGo"
  | Rent _ -> "Rent"
