open! Core
open More

module Game = struct
  module Subset = struct
    type t =
      { blue : int
      ; red : int
      ; green : int
      }
    [@@deriving fields, sexp]

    let of_string s =
      let counts = String.split_on s ~on:"," in
      List.fold counts ~init:{ blue = 0; red = 0; green = 0 } ~f:(fun t count ->
        match String.split_whitespace count with
        | [ count; "blue" ] -> { t with blue = Int.of_string count }
        | [ count; "red" ] -> { t with red = Int.of_string count }
        | [ count; "green" ] -> { t with green = Int.of_string count }
        | _ -> assert false)
    ;;

    let is_possible t = t.blue <= 14 && t.red <= 12 && t.green <= 13
    let power t = t.blue * t.red * t.green
  end

  type t =
    { id : int
    ; subsets : Subset.t list
    }
  [@@deriving sexp]

  let of_string s =
    match String.split_on s ~on:":" with
    | [ game; subsets ] ->
      let id =
        match String.split_whitespace game with
        | [ "Game"; id ] -> Int.of_string id
        | _ -> failwith game
      in
      let subsets = String.split_on subsets ~on:";" |> List.map ~f:Subset.of_string in
      { id; subsets }
    | _ -> assert false
  ;;

  let is_possible t = List.for_all t.subsets ~f:Subset.is_possible

  let minimum_set t =
    List.fold
      t.subsets
      ~init:Subset.{ blue = 0; red = 0; green = 0 }
      ~f:(fun t subset ->
        { blue = Int.max t.blue subset.blue
        ; red = Int.max t.red subset.red
        ; green = Int.max t.green subset.green
        })
  ;;
end

let read_games () = In_channel.(input_lines stdin) |> List.map ~f:Game.of_string

module A = struct
  let solve games =
    List.filter_map games ~f:(fun game ->
      match Game.is_possible game with
      | true -> Some game.id
      | false -> None)
    |> List.sum (module Int) ~f:Fn.id
  ;;

  let%expect_test _ =
    let games =
      {|Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green|}
      |> String.split_lines
      |> List.map ~f:Game.of_string
    in
    let answer = solve games in
    print_s [%message (answer : int)];
    [%expect {| (answer 8) |}]
  ;;
end

module B = struct
  let solve games =
    let minimal_sets = List.map games ~f:Game.minimum_set in
    let powers = List.map minimal_sets ~f:Game.Subset.power in
    List.sum (module Int) powers ~f:Fn.id
  ;;

  let%expect_test _ =
    let games =
      {|Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green|}
      |> String.split_lines
      |> List.map ~f:Game.of_string
    in
    let answer = solve games in
    print_s [%message (answer : int)];
    [%expect {| (answer 2286) |}]
  ;;
end

let run which =
  match which with
  | `A ->
    let games = read_games () in
    let answer = A.solve games in
    print_s [%message (answer : int)]
  | `B ->
    let games = read_games () in
    let answer = B.solve games in
    print_s [%message (answer : int)]
;;
