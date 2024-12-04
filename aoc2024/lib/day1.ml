open Core

let calculate_total_distance l r =
  let compare = Int.compare in
  let l = List.sort l ~compare in
  let r = List.sort r ~compare in
  List.map2_exn l r ~f:(fun l r -> abs (l - r)) 
  |> List.sum (module Int) ~f:Fn.id 
  |> printf "%d\n"
;;

let split_on_space s = String.split_on_chars s ~on:[' '] |> List.filter ~f:(function | "" -> false | _ -> true)

let printer fn v  = 
  print_s (fn v);
  v 
;;

let command = Command.basic
  ~summary:"day1"
  [%map_open.Command
    let () = return () in
    fun () ->
      print_endline "AOC Day1";
      let lines = In_channel.input_all (In_channel.stdin) |> String.strip |> String.split_lines in
      (* parse each line  *)
      let numbers = List.filter_map lines ~f:(function 
      | "" | "\n"-> None 
      | line -> Some (String.strip line |> split_on_space |> List.map ~f:Int.of_string)) in
      match List.transpose numbers with
      | None -> print_endline "Error - could not transpose"
      | Some [l; r] -> calculate_total_distance l r
      | _ -> print_endline "Error - not two columns of numbers"
  ]