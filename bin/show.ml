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
