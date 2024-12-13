open Core

let map_consecutive ~f elements =
  let rec iter acc = function
    | [ a; b ] -> f a b :: acc
    | a :: b :: rest -> f a b :: iter acc (b :: rest)
    | [] | [ _ ] -> failwith "cannot map_consecutive list of len < 2"
  in
  iter [] elements
;;

let is_safe record = map_consecutive ~f:(fun a b -> b - a) record
