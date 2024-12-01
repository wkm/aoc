open Core
open Async

let command = Command.group ~summary:"aoc" ["day1"; day1.command]
let() = Command_unix.run command