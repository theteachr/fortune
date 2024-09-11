module Color_unicode = struct
  let rgb = function
    | Fortune.Color.Black -> (0, 0, 0)
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

module Color_ascii = struct
  let show = function
    | Fortune.Color.Black -> "BLACK"
    | Blue -> "BLUE"
    | Brown -> "BROWN"
    | Green -> "GREEN"
    | Magenta -> "MAGENTA"
    | Orange -> "ORANGE"
    | Red -> "RED"
    | SkyBlue -> "SKYBLUE"
    | Turquoise -> "TURQUOISE"
    | Yellow -> "YELLOW"
end

module Make (Color : sig
  val show : Fortune.Color.t -> string
end) =
struct
  let read_input () =
    print_string "> ";
    match read_line () with
    | "q" -> None
    | line -> Some line

  let parse_input = Command.parse
  let update = Command.exec

  let show_indexed show items =
    items
    |> List.mapi (fun i item -> Printf.sprintf "%d. %s" i (show item))
    |> String.concat "\n"

  module Property = struct
    open Fortune.Property

    let show_dual ((a, b), choice) =
      match choice with
      | Fortune.Dual.L -> Printf.sprintf "[%s] %s" (Color.show a) (Color.show b)
      | R -> Printf.sprintf "%s [%s]" (Color.show a) (Color.show b)

    let show_card : card -> string = function
      | Simple color -> Color.show color
      | Dual (a, b) -> Printf.sprintf "%s %s" (Color.show a) (Color.show b)
      | Wild -> "WILD PROPERTY"

    let show : t -> string = function
      | Simple color -> Color.show color
      | Dual (colors, choice) -> show_dual (colors, choice)
      | Wild color -> Printf.sprintf "WILD %s" (Color.show color)
  end

  module Action = struct
    open Fortune.Action

    let show = function
      | DealBreaker -> "DEAL BREAKER"
      | ForcedDeal -> "FORCED DEAL"
      | SlyDeal -> "SLY DEAL"
      | JustSayNo -> "JUST SAY NO"
      | DebtCollector -> "DEBT COLLECTOR"
      | Birthday -> "BIRTHDAY"
      | DoubleTheRent -> "DOUBLE THE RENT"
      | Building House -> "HOUSE"
      | Building Hotel -> "HOTEL"
      | PassGo -> "PASS GO"
      | Rent (Dual (a, b)) ->
          Printf.sprintf "RENT: %s %s" (Color.show a) (Color.show b)
      | Rent Wild -> "WILD RENT"
  end

  module Money = struct
    open Fortune.Money

    let show = function
      | M value -> Printf.sprintf "M%d" value
      | Action action -> Printf.sprintf "%s" (Action.show action)
  end

  module Card = struct
    open Fortune.Card

    let show = function
      | Money money -> Printf.sprintf "M%d" money
      | Property property -> Property.show_card property
      | Action action -> Action.show action
  end

  module Player = struct
    open Fortune.Player

    let show_hand hand = show_indexed Card.show hand
    let show_properties properties = show_indexed Property.show properties
    let show_bank bank = show_indexed Money.show bank

    let show { hand; properties; bank; name } =
      Printf.sprintf {|%s

Hand -

%s

Bank -

%s

Properties -

%s|} name
        (show_hand hand) (show_bank bank)
        (show_properties properties)
  end

  let show game =
    Printf.sprintf {|%s

%d card(s) left in the deck.
|}
      (game |> Fortune.Game.current_player |> Player.show)
      (Fortune.Deck.count game.draw_pile)

  let clear_screen () = Sys.command "clear" |> ignore

  let render ?(padding = 4) game =
    clear_screen ();
    game
    |> show
    |> String.split_on_char '\n'
    |> List.map (( ^ ) @@ String.make padding ' ')
    |> String.concat "\n"
    |> print_endline
end
