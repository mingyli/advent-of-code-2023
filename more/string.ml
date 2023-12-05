open! Core
include String

let split_whitespace t = Str.(split (regexp " +")) t
let split_on t ~on = Str.(split (regexp on)) t
