let show_indexed show items =
  items
  |> List.mapi (fun i item -> Printf.sprintf "%d. %s" (i + 1) (show item))
  |> String.concat "\n  "

module Color = struct
  open Fortune.Color

  let rgb = function
    | Black -> (0, 0, 0)
    | Blue -> (10, 147, 150)
    | Brown -> (150, 75, 0)
    | Green -> (83, 221, 108)
    | Magenta -> (214, 122, 177)
    | Orange -> (255, 120, 79)
    | Red -> (232, 49, 81)
    | SkyBlue -> (132, 218, 235)
    | Turquoise -> (148, 210, 189)
    | Yellow -> (244, 233, 0)

  let show color =
    let r, g, b = rgb color in
    Printf.sprintf "\x1b[38;2;%d;%d;%dm●\x1b[0m" r g b
end

module Property = struct
  open Fortune.Property

  let show_dual ((a, b), choice) =
    match choice with
    | Dual.L -> Printf.sprintf "[%s]%s" (Color.show a) (Color.show b)
    | R -> Printf.sprintf "%s[%s]" (Color.show a) (Color.show b)

  let show_card : card -> string = function
    | Simple color -> Color.show color
    | Dual (a, b) -> Printf.sprintf "%s%s" (Color.show a) (Color.show b)
    | Wild -> "Wild"

  let show : t -> string = function
    | Simple color -> Color.show color
    | Dual (colors, choice) -> show_dual (colors, choice)
    | Wild color -> Printf.sprintf "Wild %s" (Color.show color)
end

module Action = struct
  open Fortune.Action

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
end

module Money = struct
  open Fortune.Money

  let show = function
    | M value -> Printf.sprintf "%d M" value
    | Action action -> Printf.sprintf "%s" (Action.show action)
end

module Card = struct
  open Fortune.Card

  let show = function
    | Money money -> Money.show money
    | Property property -> Property.show_card property
    | Action action -> Action.show action
end

module Player = struct
  open Fortune.Player

  let hand { hand; _ } = show_indexed Card.show hand
  let properties { properties; _ } = show_indexed Property.show properties
end

let game Fortune.Game.{ draw_pile; players; _ } =
  let player = Fortune.Game.Round.current players in
  Printf.sprintf
    {|
  ==== MONOPOLY DEAL ====

  %s is playing.

  Hand -

  %s

  Properties -

  %s

  %d card(s) left in the deck.
|}
    player.name (Player.hand player) (Player.properties player)
    (Fortune.Deck.count draw_pile)
