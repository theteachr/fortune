include Fortune.Player

let hand { hand; _ } = hand |> List.map Card.show |> String.concat "\n  "

let properties { properties; _ } =
  properties |> List.map Property.show |> String.concat "\n  "
