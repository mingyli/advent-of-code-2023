open! Core
open Src

module Which = struct
  type t =
    [ `A
    | `B
    ]
  [@@deriving enumerate, sexp]

  let arg_type = Command.Arg_type.of_alist_exn [ "a", `A; "A", `A; "b", `B; "B", `B ]
end

let command =
  Command.basic ~summary:"Advent of Code 2023"
  @@
  let%map_open.Command () = return ()
  and day = anon @@ ("day" %: int)
  and which = anon @@ ("which" %: Which.arg_type) in
  fun () ->
    let run =
      match day with
      | 1 -> Day1.run
      | 2 -> Day2.run
      | 3 -> Day3.run
      | 4 -> Day4.run
      | _ -> assert false
    in
    run which
;;

let () = Command.run command
