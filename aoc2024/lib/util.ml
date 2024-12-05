open Core

let read_integer_file fname =
  let per_line line =
    String.strip line
    |> String.split ~on:' '
    |> List.filter_map ~f:(function
      | "" -> None
      | s -> Some (Int.of_string s))
  in
  In_channel.read_lines fname |> List.map ~f:per_line
;;

let%expect_test _ =
  let data = read_integer_file "/Users/wkm/Code/aoc/aoc2024/data/day2" in
  (* print first five lines *)
  List.take data 5 |> List.iter ~f:(fun e -> List.sexp_of_t Int.sexp_of_t e |> print_s);
  [%expect
    {|
    (3 6 7 9 11 8)
    (21 24 25 26 29 30 32 32)
    (29 32 33 34 35 37 38 42)
    (54 55 57 58 60 61 63 70)
    (59 61 60 63 65 68 71)
    |}]
;;
