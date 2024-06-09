type rent

type t =
  | DealBreaker
  | ForcedDeal
  | SlyDeal
  | JustSayNo
  | DebtCollector
  | Birthday
  | DoubleTheRent
  | House
  | Hotel
  | PassGo
  | Rent of rent

type target =
  | Single
  | All

type effect = 
  | Draw of int
  | Rent of {
    amount: int;
    target: target;
  }

let play = function
  | DealBreaker -> failwith "todo"
  | ForcedDeal -> failwith "todo"
  | SlyDeal -> failwith "todo"
  | JustSayNo -> failwith "todo"
  | DebtCollector -> failwith "todo"
  | Birthday -> failwith "todo"
  | DoubleTheRent -> failwith "todo"
  | House -> failwith "todo"
  | Hotel -> failwith "todo"
  | PassGo -> failwith "todo"
  | Rent _ -> failwith "todo"

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
