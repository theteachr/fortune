type t = Card.t list

let count = List.length
let ( *. ) n card = List.init n (Fun.const card)

let shuffle ?seed d =
  (match seed with
  | None -> Random.self_init ()
  | Some n -> Random.init n);
  d
  |> List.map (fun c -> (Random.bits (), c))
  |> List.sort compare
  |> List.map snd

let default =
  let properties =
    let ( * ) n color = n *. Card.simple_property color in
    [
      2 * Blue;
      2 * Brown;
      3 * Green;
      3 * SkyBlue;
      3 * Orange;
      3 * Magenta;
      4 * Black;
      3 * Red;
      2 * Turquoise;
      3 * Yellow;
    ]
  in
  let monies =
    let ( * ) n value = n *. Card.money value in
    [ 2 * 5; 3 * 4; 3 * 3; 5 * 2; 6 * 1; 1 * 10 ]
  in
  let actions =
    let ( * ) n action = n *. Card.action action in
    [
      2 * DealBreaker;
      3 * JustSayNo;
      3 * SlyDeal;
      3 * ForcedDeal;
      3 * DebtCollector;
      3 * Birthday;
      3 * Building House;
      2 * Building Hotel;
      2 * DoubleTheRent;
      10 * PassGo;
    ]
  in
  let wild_properties =
    let ( * ) n colors = n *. Card.dual_property colors in
    [
      1 * (SkyBlue, Brown);
      1 * (SkyBlue, Black);
      2 * (Magenta, Orange);
      2 * (Yellow, Red);
      1 * (Blue, Green);
      1 * (Green, Black);
      1 * (Black, Turquoise);
      2 *. Card.wild_property;
    ]
  in
  let wild_rents =
    let ( * ) n colors = n *. Card.rent colors in
    [
      2 * (Green, Blue);
      2 * (Brown, SkyBlue);
      2 * (Magenta, Orange);
      2 * (Black, Turquoise);
      2 * (Red, Yellow);
      3 *. Card.wild_rent;
    ]
  in
  properties @ monies @ actions @ wild_properties @ wild_rents |> List.flatten

let take n deck =
  let rec take' n cards = function
    | deck when n = 0 -> (cards, deck)
    | [] -> (cards, [])
    | card :: rest -> take' (n - 1) (card :: cards) rest
  in
  take' n [] deck
