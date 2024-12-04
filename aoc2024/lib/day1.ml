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

let command =
  Command.basic
    ~summary:"day1"
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
        | Some [ l; r ] -> calculate_total_distance l r
        | _ -> print_endline "Error - not two columns of numbers"]
;;
