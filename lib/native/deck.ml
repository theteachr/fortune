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
    let ( * ) n color = n *. Card.make_simple_property color in
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
    let ( * ) n value = n *. Card.make_money value in
    [ 2 * 5; 3 * 4; 3 * 3; 5 * 2; 6 * 1; 1 * 10 ]
  in
  let actions =
    let ( * ) n action = n *. Card.make_action action in
    [
      2 * Deal_breaker;
      3 * Just_say_no;
      3 * Sly_deal;
      3 * Forced_deal;
      3 * Debt_collector;
      3 * Birthday;
      2 * Double_the_rent;
      10 * Pass_go;
    ]
  in
  let buildings =
    let ( * ) n building = n *. Card.make_building building in
    [ 3 * House; 2 * Hotel ]
  in
  let wild_properties =
    let ( * ) n colors = n *. Card.make_dual_property colors in
    [
      1 * (SkyBlue, Brown);
      1 * (SkyBlue, Black);
      2 * (Magenta, Orange);
      2 * (Yellow, Red);
      1 * (Blue, Green);
      1 * (Green, Black);
      1 * (Black, Turquoise);
      2 *. Card.make_wild_property;
    ]
  in
  let wild_rents =
    let ( * ) n colors = n *. Card.make_rent colors in
    [
      2 * (Green, Blue);
      2 * (Brown, SkyBlue);
      2 * (Magenta, Orange);
      2 * (Black, Turquoise);
      2 * (Red, Yellow);
      3 *. Card.make_wild_rent;
    ]
  in
  properties @ monies @ actions @ buildings @ wild_properties @ wild_rents
  |> List.flatten
  |> List.mapi (fun id kind -> Card.{ id; kind })

let take' n deck =
  let rec take' n cards = function
    | deck when n = 0 -> (cards, Either.Right deck)
    | [] -> (cards, Either.left n)
    | card :: rest -> take' (n - 1) (card :: cards) rest
  in
  take' n [] deck

let take n deck =
  let cards, deck = take' n deck in
  (cards, Either.fold ~left:(Fun.const []) ~right:Fun.id deck)

let add card deck = card :: deck
let of_list cards = cards
