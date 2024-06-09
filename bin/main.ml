open Fortune

let () =
  let players = [
    "theteachr";
    "injuly";
    "kaus";
  ] |> List.map Player.create |> Game.Round.of_list
  in
  Game.(display {
    draw_pile = Deck.default;
    play_pile = [];
    players;
  })
