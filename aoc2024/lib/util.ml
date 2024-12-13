open Core

let split_on_space s =
  String.split_on_chars s ~on:[ ' ' ]
  |> List.filter ~f:(function
    | "" -> false
    | _ -> true)
;;

let printer fn v =
  print_s (fn v);
  v
;;
