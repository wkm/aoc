open Core

let calculate_total_distance l r =
  (* count number of instances *)
  let counts = Hashtbl.create (module Int) in
  List.iter r ~f:(fun i -> Hashtbl.incr counts i);
  List.fold l ~init:0 ~f:(fun acc i ->
    match Hashtbl.find counts i with
    | None -> acc
    | Some c -> acc + (i * c))
  |> printf "%d\n"
;;

let%expect_test _ =
  let d =
    Util.read_string {|
    3   4
    4   3
    2   5
    1   3
    3   9
    3   3|}
  in
  Util.print_int_list_list d;
  [%expect {|
    (3 4)
    (4 3)
    (2 5)
    (1 3)
    (3 9)
    (3 3)
    |}]
;;
