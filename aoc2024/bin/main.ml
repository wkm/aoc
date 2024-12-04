open Async
open Core

let command = Command.group ~summary:"aoc" ["day1"; day1.command]
let() = Command_unix.run command