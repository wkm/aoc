open Core

(* find each instance of 'X' and then search around it for each of the [XMAS] permutations *)
let sample_data =
  {|
MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX
|}
;;

let array_of_data data =
  String.strip data |> String.split_lines |> List.map ~f:String.to_array |> Array.of_list
;;

let search_term = "XMAS" |> String.to_array
let directions = [ 1, 0; 1, 1; 0, 1; -1, 1; -1, 0; -1, -1; 0, -1; 1, -1 ]

let search_direction grid x y dx dy =
  Array.for_alli search_term ~f:(fun i c ->
    let x = x + (i * dx) in
    let y = y + (i * dy) in
    if x < 0 || y < 0 || x >= Array.length grid || y >= Array.length grid.(0)
    then false
    else Char.equal c grid.(x).(y))
;;

let search_spot grid x y =
  List.fold directions ~init:0 ~f:(fun acc (dx, dy) ->
    if search_direction grid x y dx dy then acc + 1 else acc)
;;

let search grid =
  Array.foldi grid ~init:0 ~f:(fun x acc row ->
    acc + Array.foldi row ~init:0 ~f:(fun y acc _ -> acc + search_spot grid x y))
;;

let%expect_test "part 1 sample" =
  sample_data |> array_of_data |> search |> printf "%d\n";
  [%expect {| 18 |}]
;;

let%expect_test "part 1 submittal" =
  In_channel.read_all "/Users/wkm/Code/aoc/aoc2024/data/day4"
  |> array_of_data
  |> search
  |> printf "%d\n";
  [%expect {| 2639 |}]
;;
