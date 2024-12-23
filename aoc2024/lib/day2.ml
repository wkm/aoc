open Core

let map_consecutive ~f elements =
  let rec iter acc = function
    | [ a; b ] -> f a b :: acc
    | a :: b :: rest -> f a b :: iter acc (b :: rest)
    | [] | [ _ ] -> failwith "cannot map_consecutive list of len < 2"
  in
  iter [] elements
;;

let is_safe_already record =
  let deltas = map_consecutive ~f:(fun a b -> b - a) record in
  let safe_negative = List.for_all deltas ~f:(fun d -> -3 <= d && d <= -1) in
  let safe_positive = List.for_all deltas ~f:(fun d -> 1 <= d && d <= 3) in
  safe_negative || safe_positive
;;

let sample_data = {|
7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9
|}

let%expect_test "part 1 sample" =
  let safeties =
    Util.read_string sample_data |> List.map ~f:(fun r -> is_safe_already r)
  in
  List.iter safeties ~f:(fun s -> print_endline (Bool.to_string s));
  [%expect {|
    true
    false
    false
    false
    false
    true
    |}];
  List.count safeties ~f:Fn.id |> printf "%d\n";
  [%expect {|
    2
    |}]
;;

let%expect_test "part 1 submittal" =
  let data = Util.read_integer_file "/Users/wkm/Code/aoc/aoc2024/data/day2" in
  List.count data ~f:is_safe_already |> printf "%d\n";
  [%expect {| 686 |}]
;;

let without l n = List.filteri l ~f:(fun i _ -> i <> n)

let could_be_safe record =
  is_safe_already record
  || (* attempt to drop each element and then check if the record is safe *)
  List.existsi record ~f:(fun i _ -> without record i |> is_safe_already)
;;

let%expect_test "part 2 sample" =
  let safeties = Util.read_string sample_data |> List.map ~f:(fun r -> could_be_safe r) in
  List.iter safeties ~f:(fun s -> print_endline (Bool.to_string s));
  [%expect {|
    true
    false
    false
    true
    true
    true
    |}];
  List.count safeties ~f:Fn.id |> printf "%d\n";
  [%expect {| 4 |}]
;;

let%expect_test "part 2 submittal" =
  let data = Util.read_integer_file "/Users/wkm/Code/aoc/aoc2024/data/day2" in
  List.count data ~f:could_be_safe |> printf "%d\n";
  [%expect {| 717 |}]
;;
