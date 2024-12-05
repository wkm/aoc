open Async
open Core

let command = Command.group ~summary:"aoc" [ "day1", Aoc2024.Day1.command ]
let () = Command_unix.run command
