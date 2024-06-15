type t = Card.t list

let count = List.length
let ( * ) n card = List.init n (Fun.const card)
let p color = Card.Property (Simple color)
let m value = Card.Money (M value)
let a action = Card.Action action
let dual a b = Card.Property (Dual (a, b))
let dual_rent a b = Card.Action (Rent (Dual (a, b)))

let shuffle d =
  Random.self_init ();
  d
  |> List.map (fun c -> (Random.bits (), c))
  |> List.sort compare
  |> List.map snd

let default =
  [
    (* Properties *)
    2 * p Blue;
    2 * p Brown;
    3 * p Green;
    3 * p SkyBlue;
    3 * p Orange;
    3 * p Magenta;
    4 * p Black;
    3 * p Red;
    2 * p Turquoise;
    3 * p Yellow;
    (* Monies *)
    2 * m 5;
    3 * m 4;
    3 * m 3;
    5 * m 2;
    6 * m 1;
    1 * m 10;
    (* Actions *)
    2 * a DealBreaker;
    3 * a JustSayNo;
    3 * a SlyDeal;
    4 * a ForcedDeal;
    3 * a DebtCollector;
    3 * a Birthday;
    3 * a (Building House);
    3 * a (Building Hotel);
    2 * a DoubleTheRent;
    10 * a PassGo;
    (* Wild Properties *)
    2 * Card.Property Wild;
    4 * dual Blue Green;
    1 * dual Turquoise Brown;
    4 * dual Green Black;
    4 * dual SkyBlue Black;
    2 * dual Turquoise Black;
    2 * dual Orange Black;
    2 * dual Yellow Red;
    (* Rent Actions *)
    3 * a (Rent Wild);
    2 * dual_rent Green Blue;
    2 * dual_rent Brown SkyBlue;
    2 * dual_rent Magenta Orange;
    2 * dual_rent Black Turquoise;
    2 * dual_rent Red Yellow;
  ]
  |> List.flatten
  |> shuffle

let take n deck =
  let rec take' n cards = function
    | deck when n = 0 -> (cards, deck)
    | [] -> (cards, [])
    | card :: rest -> take' (n - 1) (card :: cards) rest
  in
  take' n [] deck
