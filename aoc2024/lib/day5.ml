open Core

let sample_data =
  {|
47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47
|}
;;

let parse_rule line =
  match String.split line ~on:'|' with
  | [ a; b ] -> Some (Int.of_string a, Int.of_string b)
  | _ -> None
;;

let parse_updates line =
  match String.split line ~on:',' with
  | _ :: _ :: _ as l -> Some (List.map l ~f:Int.of_string)
  | _ -> None
;;

let collect_util items ~f =
  let rec iter acc = function
    | [] -> List.rev acc, []
    | hd :: tl ->
      (match f hd with
       | None -> acc, tl
       | Some i -> iter (i :: acc) tl)
  in
  iter [] items
;;

let parse_data s =
  let lines = String.strip s |> String.split_lines in
  let rules, rest = collect_util lines ~f:parse_rule in
  let updates, _ = collect_util rest ~f:parse_updates in
  let rules =
    Map.of_alist_multi (module Int) rules
    |> Map.map ~f:(fun r -> Set.of_list (module Int) r)
  in
  let updates = List.map ~f:Array.of_list updates in
  rules, updates
;;

let test_update rules update =
  (* for each entry in the update, ensure each prior number does not fail *)
  Array.for_alli update ~f:(fun i n ->
    let rls = Map.find rules n |> Option.value ~default:Int.Set.empty in
    Array.sub update ~pos:0 ~len:i
    |> Array.for_all ~f:(fun elem -> not (Set.mem rls elem)))
;;

let get_middle update =
  let middle = Array.length update / 2 in
  update.(middle)
;;

let%expect_test "part 1 test" =
  let rules, updates = parse_data sample_data in
  let update = List.hd_exn updates in
  let result = test_update rules update in
  print_s [%message (rules : Int.Set.t Int.Map.t) (update : int array) (result : bool)];
  [%expect
    {|
    ((rules
      ((29 (13)) (47 (13 29 53 61)) (53 (13 29)) (61 (13 29 53))
       (75 (13 29 47 53 61)) (97 (13 29 47 53 61 75))))
     (update (75 47 61 53 29)) (result true))
    |}];
  let res =
    List.sum
      (module Int)
      updates
      ~f:(fun update -> if test_update rules update then get_middle update else 0)
  in
  printf "%d" res;
  [%expect {| 143 |}]
;;

let%expect_test "part 1 submittal" =
  let data = In_channel.read_all "/Users/wkm/Code/aoc/aoc2024/data/day5" in
  let rules, updates = parse_data data in
  let res =
    List.sum
      (module Int)
      updates
      ~f:(fun update -> if test_update rules update then get_middle update else 0)
  in
  printf "%d" res;
  [%expect {| 5964 |}]
;;
