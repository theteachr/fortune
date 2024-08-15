let read_command () =
  print_string "> ";
  () |> read_line |> Command.parse |> function
  | Quit -> None
  | command -> Some command

