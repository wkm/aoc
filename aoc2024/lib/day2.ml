open Core

let pairwise_map l ~f =
  let rec loop acc = function
    | [] -> []
    | [ x ] -> [ x ]
    | x :: y :: rst -> f x y :: loop acc (y :: rst)
  in
  loop [] l
;;

let is_safe report =
  (* safe if all differences share a sign and all differences are in (1, 2, 3) *)
  pairwise_map report ~f:(fun a b -> b - a)
;;

let command =
  Command.basic
    ~summary:"day2"
    [%map_open.Command
      let () = return () in
      fun () ->
        print_endline "AOC Day1";
        let lines =
          In_channel.input_all In_channel.stdin |> String.strip |> String.split_lines
        in
        (* parse each line  *)
        let numbers =
          List.filter_map lines ~f:(function
            | "" | "\n" -> None
            | line ->
              Some (String.strip line |> Util.split_on_space |> List.map ~f:Int.of_string))
        in
        match List.transpose numbers with
        | None -> print_endline "Error - could not transpose"
        | Some [ l; r ] ->
          ignore l;
          ignore r;
          failwith "todo" (*calculate_total_distance l r*)
        | _ -> print_endline "Error - not two columns of numbers"]
;;
