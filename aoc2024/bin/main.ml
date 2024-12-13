open Core
open Aoc2024

let command = Command.group ~summary:"aoc" [ "day1", Day1.command; "day2", Day2.command ]
let () = Command_unix.run command
