include Property

(* let show_dual ((a, b), choice) = *)
(*   match choice with *)
(*   | Dual.L -> Printf.sprintf "[%s]%s" (Color.show a) (Color.show b) *)
(*   | R -> Printf.sprintf "%s[%s]" (Color.show a) (Color.show b) *)

(* let show_card : card -> string = function *)
(*   | Simple color -> Color.show color *)
(*   | Dual (a, b) -> Printf.sprintf "%s%s" (Color.show a) (Color.show b) *)
(*   | Wild -> "Wild" *)

(* let show : t -> string = function *)
(*   | Simple color -> Color.show color *)
(*   | Dual (colors, choice) -> show_dual (colors, choice) *)
(*   | Wild color -> Printf.sprintf "Wild %s" (Color.show color) *)
