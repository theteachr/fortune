type t = {
  game: Fortune.Game.t;
  error_message: string option;
}

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

let show_indexed show items =
  items
  |> List.mapi (fun i item -> Printf.sprintf "%d. %s" i (show item))
  |> String.concat "\n"

let show_items show items = items |> List.map show |> String.concat "\n"

module Make (Color : sig
  val show : Fortune.Color.t -> string
end) =
struct
  type nonrec t = t

  let init game = { game; error_message = None }

  let read_input () =
    print_string "> ";
    match read_line () with
    | "q" -> None
    | line -> Some line

  module Property = struct
    open Fortune.Property

    let show_dual ((a, b), choice) =
      match choice with
      | Fortune.Dual.L -> Printf.sprintf "[%s] %s" (Color.show a) (Color.show b)
      | R -> Printf.sprintf "%s [%s]" (Color.show a) (Color.show b)

    let show_card : card -> string = function
      | Simple color -> Color.show color
      | Dual (a, b) -> Printf.sprintf "%s %s" (Color.show a) (Color.show b)
      | Wild _ -> "WILD PROPERTY"

    let show : t -> string = function
      | Simple color -> Color.show color
      | Dual (colors, choice) -> show_dual (colors, choice)
      | Wild color -> Printf.sprintf "%s [WILD]" (Color.show color)
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
      | Action action ->
          Printf.sprintf "M%d (%s)"
            (Fortune.Action.value action)
            (Action.show action)
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

    let show_hand hand =
      show_items
        (fun (id, card) -> Printf.sprintf "%d. %s" id (Card.show card))
        (Hand.to_list hand)

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

  let show_used cards =
    cards
    |> List.map (function
         | Fortune.Card.Used.Property p -> Property.show p
         | Action a -> Action.show a
         | Money m -> Money.show m)
    |> String.concat "\n"

  let show { game; error_message } =
    Printf.sprintf {|%s

This turn -

%s

%d card(s) left in the deck.

[%s]
|}
      (game |> Fortune.Game.current_player |> Player.show)
      (show_used game.played_cards)
      (Fortune.Deck.count game.draw_pile)
      (error_message |> Option.value ~default:"")

  let clear_screen () = Sys.command "clear" |> ignore

  let render ?(padding = 4) ui =
    clear_screen ();
    ui
    |> show
    |> String.split_on_char '\n'
    |> List.map (( ^ ) @@ String.make padding ' ')
    |> String.concat "\n"
    |> print_endline
end
