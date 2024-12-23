open Core

let part1_sample =
  {|xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))|}
;;

(* parse the integer and give the remaining string, or [None] *)

let parse_digit = function
  | (('0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9') as d) :: rst ->
    Some d, rst
  | rst -> None, rst
;;

let parse_num code =
  let rec step acc code =
    match acc, parse_digit code with
    | [], (None, rst) -> [], rst
    | acc, (Some d, rst) -> step (d :: acc) rst
    | acc, (None, rst) -> acc, rst
  in
  match step [] code with
  | [], rst -> None, rst
  | d, rst -> Some (List.rev d |> String.of_char_list |> Int.of_string), rst
;;

let%expect_test "parsing" =
  let tst case =
    let r = parse_num (case |> String.to_list) in
    print_s ([%sexp_of: int option * char list] r)
  in
  tst "123foo";
  [%expect {| ((123) (f o o)) |}];
  tst "foo123";
  [%expect {| (() (f o o 1 2 3)) |}]
;;

let parse_mul = function
  | 'm' :: 'u' :: 'l' :: '(' :: rst -> Some (), rst
  | rst -> None, rst
;;

let parse_comma = function
  | ',' :: rst -> Some (), rst
  | rst -> None, rst
;;

let parse_end = function
  | ')' :: rst -> Some (), rst
  | rst -> None, rst
;;

let parse_mult code =
  match parse_mul code with
  | None, rst -> None, rst
  | Some (), rst ->
    (match parse_num rst with
     | None, rst -> None, rst
     | Some lhs, rst ->
       (match parse_comma rst with
        | None, rst -> None, rst
        | Some (), rst ->
          (match parse_num rst with
           | None, rst -> None, rst
           | Some rhs, rst ->
             (match parse_end rst with
              | None, _ -> None, rst
              | Some (), rst -> Some (lhs * rhs), rst))))
;;

let parse_all_mults code =
  let rec step acc rst =
    match parse_mult rst with
    | None, [] | None, [ _ ] -> acc
    | None, _ :: rst -> step acc rst
    | Some i, rst -> step (acc + i) rst
  in
  step 0 code
;;

let%expect_test "part 1 test" =
  parse_all_mults (part1_sample |> String.to_list) |> printf "%d\n";
  [%expect {| 161 |}]
;;

let%expect_test "part 1 submittal" =
  let data =
    In_channel.read_all "/Users/wkm/Code/aoc/aoc2024/data/day3" |> String.to_list
  in
  parse_all_mults data |> printf "%d\n";
  [%expect {| 187825547 |}]
;;

module State = struct
  type t =
    { count : int
    ; enabled : bool
    }
  [@@deriving sexp]

  let initial = { count = 0; enabled = true }
end

let parse_enabled = function
  | 'd' :: 'o' :: '(' :: ')' :: rst -> Some (), rst
  | rst -> None, rst
;;

let parse_disabled = function
  | 'd' :: 'o' :: 'n' :: '\'' :: 't' :: '(' :: ')' :: rst -> Some (), rst
  | rst -> None, rst
;;

let parse_all_mults_and_states code =
  let initial = State.initial in
  let rec step (s : State.t) rst =
    match parse_enabled rst with
    | Some (), rst -> step { s with enabled = true } rst
    | None, rst ->
      (match parse_disabled rst with
       | Some (), rst -> step { s with enabled = false } rst
       | None, rst ->
         (match parse_mult rst with
          | Some count, rst when s.enabled -> step { s with count = s.count + count } rst
          | Some _, rst -> step s rst
          | None, _ :: rst -> step s rst
          | None, [] -> s))
  in
  let { State.count; _ } = step initial code in
  count
;;

let part2_sample =
  {|xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))|}
;;

let%expect_test "part 2 test" =
  parse_all_mults_and_states (part2_sample |> String.to_list) |> printf "%d\n";
  [%expect {| 48 |}]
;;

let%expect_test "part 2 submittal" =
  let data =
    In_channel.read_all "/Users/wkm/Code/aoc/aoc2024/data/day3" |> String.to_list
  in
  parse_all_mults_and_states data |> printf "%d\n";
  [%expect {| 85508223 |}]
;;
