type t = {
  game: Fortune.Game.t;
  error_message: string option;
}

open Printf

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
    sprintf "\x1b[38;2;%d;%d;%dm●\x1b[0m" r g b
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
  |> List.mapi (fun i item -> sprintf "%d. %s" i (show item))
  |> String.concat "\n"

let show_items show items = items |> List.map show |> String.concat "\n"

module Make (Color : sig
  val show : Fortune.Color.t -> string
end) =
struct
  type nonrec t = t

  let init game = { game; error_message = None }
  let game_over { game; _ } = Fortune.Game.is_over game

  let read_input () =
    print_string "> ";
    match read_line () with
    | "q" -> None
    | line -> Some line

  module Dual = struct
    let show_active ((a, b), choice) =
      match choice with
      | Fortune.Dual.L -> sprintf "[%s] %s" (Color.show a) (Color.show b)
      | R -> sprintf "%s [%s]" (Color.show a) (Color.show b)

    let show_inactive (a, b) = sprintf "%s %s" (Color.show a) (Color.show b)
  end

  module Property = struct
    open Fortune.Property

    let show_card : card -> string = function
      | Simple color -> Color.show color
      | Dual d -> Dual.show_inactive d
      | Wild _ -> "WILD PROPERTY"

    let show : t -> string = function
      | Simple color -> Color.show color
      | Dual d -> Dual.show_active d
      | Wild color -> sprintf "%s [WILD]" (Color.show color)
  end

  module Action = struct
    open Fortune.Action

    let show_action = function
      | Deal_breaker -> "DEAL BREAKER"
      | Forced_deal -> "FORCED DEAL"
      | Sly_deal -> "SLY DEAL"
      | Just_say_no -> "JUST SAY NO"
      | Debt_collector -> "DEBT COLLECTOR"
      | Birthday -> "BIRTHDAY"
      | Double_the_rent -> "DOUBLE THE RENT"
      | Pass_go -> "PASS GO"

    let show_building = function
      | House -> "HOUSE"
      | Hotel -> "HOTEL"

    let show = function
      | Action a -> show_action a
      | Building b -> show_building b
      | Rent (Dual d) -> sprintf "RENT: %s" (Dual.show_inactive d)
      | Rent Wild -> "WILD RENT"

    let show_used = function
      | Used.Regular action -> show_action action
      | Rent (Wild color) -> sprintf "WILD RENT: %s" (Color.show color)
      | Rent (Dual d) -> Dual.show_active d
  end

  module Money = struct
    open Fortune.Money

    let show = function
      | M value -> sprintf "M%d" value
      | Action action ->
          sprintf "M%d (%s)" (Fortune.Action.value action) (Action.show action)
  end

  module Card = struct
    open Fortune.Card

    let show = function
      | Money money -> sprintf "M%d" money
      | Property property -> Property.show_card property
      | Action action -> Action.show action
  end

  module Player = struct
    open Fortune.Player

    let show_hand hand =
      show_items
        (fun (id, card) -> sprintf "%d. %s" id (Card.show card))
        (Hand.to_list hand)

    let show_properties properties = show_indexed Property.show properties
    let show_bank bank = show_indexed Money.show bank

    let show { hand; properties; bank; name } =
      sprintf {|%s

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
         | Action a -> Action.show_used a
         | Money m -> Money.show m)
    |> String.concat "\n"

  let show { game; error_message } =
    sprintf
      {|%s

This turn -

%s

%d card(s) left in the deck.
%d card(s) in the play pile.

[%s]
|}
      (game |> Fortune.Game.current_player |> Player.show)
      (show_used game.played_cards)
      (Fortune.Deck.count game.draw_pile)
      (List.length game.play_pile)
      (error_message |> Option.value ~default:"")

  let clear_screen () = Sys.command "clear" |> ignore

  let render ?(padding = 4) ui =
    clear_screen ();
    ui
    |> show
    |> String.split_on_char '\n'
    |> List.map (sprintf "%s%s" @@ String.make padding ' ')
    |> String.concat "\n"
    |> print_endline
end
