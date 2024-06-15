type t = Card.t list

let count = List.length
let ( *. ) n card = List.init n (Fun.const card)
let repeat f n x = List.init n (Fun.const @@ f x)

let shuffle d =
  Random.self_init ();
  d
  |> List.map (fun c -> (Random.bits (), c))
  |> List.sort compare
  |> List.map snd

let default =
  let properties =
    let ( * ) = repeat (fun color -> Card.Property (Simple color)) in
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
    let ( * ) = repeat (fun value -> Card.Money (M value)) in
    [ 2 * 5; 3 * 4; 3 * 3; 5 * 2; 6 * 1; 1 * 10 ]
  in
  let actions =
    let ( * ) = repeat (fun action -> Card.Action action) in
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
    let ( * ) = repeat (fun (a, b) -> Card.Property (Dual (a, b))) in
    [
      1 * (SkyBlue, Brown);
      1 * (SkyBlue, Black);
      2 * (Magenta, Orange);
      2 * (Yellow, Red);
      1 * (Blue, Green);
      1 * (Green, Black);
      1 * (Black, Turquoise);
      2 *. Card.Property Wild;
    ]
  in
  let wild_rents =
    let ( * ) = repeat (fun (a, b) -> Card.Action (Rent (Dual (a, b)))) in
    [
      2 * (Green, Blue);
      2 * (Brown, SkyBlue);
      2 * (Magenta, Orange);
      2 * (Black, Turquoise);
      2 * (Red, Yellow);
      3 *. Card.Action (Rent Wild);
    ]
  in
  properties @ monies @ actions @ wild_properties @ wild_rents
  |> List.flatten
  |> shuffle

let take n deck =
  let rec take' n cards = function
    | deck when n = 0 -> (cards, deck)
    | [] -> (cards, [])
    | card :: rest -> take' (n - 1) (card :: cards) rest
  in
  take' n [] deck
