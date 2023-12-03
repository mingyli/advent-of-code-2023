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
      | _ -> assert false
    in
    run which
;;

let () = Command.run command
