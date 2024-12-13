open Core

let read_string string =
  let per_line = function
    | "" -> None
    | line ->
      String.strip line
      |> String.split ~on:' '
      |> List.filter_map ~f:(function
        | "" | "\n" -> None
        | s -> Some (Int.of_string s))
      |> Option.some
  in
  String.split_lines string |> List.filter_map ~f:per_line
;;

let read_integer_file fname = In_channel.read_all fname |> read_string

let print_int_list_list l =
  List.iter l ~f:(fun e -> List.sexp_of_t Int.sexp_of_t e |> print_s)
;;

let%expect_test _ =
  let data = read_integer_file "/Users/wkm/Code/aoc/aoc2024/data/day2" in
  (* print first five lines *)
  List.take data 5 |> print_int_list_list;
  [%expect
    {|
    (3 6 7 9 11 8)
    (21 24 25 26 29 30 32 32)
    (29 32 33 34 35 37 38 42)
    (54 55 57 58 60 61 63 70)
    (59 61 60 63 65 68 71)
    |}]
;;
