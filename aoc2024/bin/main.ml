open Core

let command = Command.group ~summary:"aoc" []
let () = Command_unix.run command
