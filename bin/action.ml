include Fortune.Action

let show = function
  | DealBreaker -> "DealBreaker"
  | ForcedDeal -> "ForcedDeal"
  | SlyDeal -> "SlyDeal"
  | JustSayNo -> "JustSayNo"
  | DebtCollector -> "DebtCollector"
  | Birthday -> "Birthday"
  | DoubleTheRent -> "DoubleTheRent"
  | House -> "House"
  | Hotel -> "Hotel"
  | PassGo -> "PassGo"
  | Rent _ -> "Rent"
