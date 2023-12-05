open! Core
open More

module Number = struct
  type t =
    { value : int
    ; position : Vec2.t
    }
  [@@deriving sexp, fields]

  let create value position = { value; position }
  let left_pos t = t.position

  let right_pos t =
    let width =
      match t.value with
      | v when 1 <= v && v <= 9 -> 1
      | v when 10 <= v && v <= 99 -> 2
      | v when 100 <= v && v <= 999 -> 3
      | _ -> assert false
    in
    let r, c = t.position in
    r, c + width - 1
  ;;

  let is_adjacent_to t pos =
    let r, c = t.position in
    let _, c' = right_pos t in
    let pr, pc = pos in
    let adjacent_row =
      match pr - r with
      | -1 | 0 | 1 -> true
      | _ -> false
    in
    let adjacent_col = c - 1 <= pc && pc <= c' + 1 in
    adjacent_row && adjacent_col
  ;;
end

module Schematic = struct
  type t = string array

  let of_string s = String.split_lines s |> Array.of_list

  let numbers_in_row s =
    Sequence.unfold_step ~init:0 ~f:(fun c ->
      if c = String.length s
      then Done
      else (
        let ch = String.get s c in
        match ch with
        | '.' -> Skip (c + 1)
        | digit when not (Char.is_digit digit) -> Skip (c + 1)
        | _ ->
          (match Str.split (Str.regexp "[^0-9]+") (String.drop_prefix s c) with
           | number :: _ ->
             let position = c in
             let c = c + String.length number in
             let number = Int.of_string number in
             Yield ((position, number), c)
           | _ -> assert false)))
  ;;

  let numbers t =
    Array.mapi t ~f:(fun r row ->
      numbers_in_row row
      |> Sequence.map ~f:(fun (c, number) -> (r, c), number)
      |> Sequence.to_array)
    |> Array.to_list
    |> Array.concat
    |> Array.to_list
    |> List.map ~f:(fun (pos, number) -> Number.create number pos)
  ;;

  let symbols_in_row s =
    Sequence.unfold_step ~init:0 ~f:(fun c ->
      if c = String.length s
      then Done
      else (
        let ch = String.get s c in
        match ch with
        | '.' -> Skip (c + 1)
        | digit when Char.is_digit digit -> Skip (c + 1)
        | _ -> Yield ((ch, c), c + 1)))
  ;;

  let symbols t =
    Array.mapi t ~f:(fun r row ->
      symbols_in_row row
      |> Sequence.map ~f:(fun (ch, c) -> (r, c), ch)
      |> Sequence.to_array)
    |> Array.to_list
    |> Array.concat
    |> Array.to_list
    |> Vec2.Table.of_alist_exn
  ;;
end

let read_schematic () = In_channel.(input_all stdin) |> Schematic.of_string

module A = struct
  let solve schematic =
    let numbers = Schematic.numbers schematic in
    let symbols = Schematic.symbols schematic in
    numbers
    |> List.filter_map ~f:(fun number ->
      match
        Hashtbl.existsi symbols ~f:(fun ~key ~data:_ -> Number.is_adjacent_to number key)
      with
      | false -> None
      | true -> Some number)
    |> List.sum (module Int) ~f:Number.value
  ;;

  let%expect_test _ =
    let schematic =
      {|467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..|}
      |> Schematic.of_string
    in
    let answer = solve schematic in
    print_s [%message (answer : int)];
    [%expect {| (answer 4361) |}]
  ;;
end

module B = struct
  let solve schematic =
    let numbers = Schematic.numbers schematic in
    let symbols = Schematic.symbols schematic in
    let gear_ratios = Deque.create () in
    symbols
    |> Hashtbl.iteri ~f:(fun ~key:pos ~data:symbol ->
      match symbol with
      | '*' ->
        let adjacent_numbers =
          List.filter numbers ~f:(fun number -> Number.is_adjacent_to number pos)
        in
        (match List.length adjacent_numbers with
         | 2 ->
           let gear_ratio =
             List.map adjacent_numbers ~f:(fun n -> n.value)
             |> List.reduce_exn ~f:Int.( * )
           in
           Deque.enqueue_back gear_ratios gear_ratio
         | _ -> ())
      | _ -> ());
    gear_ratios |> Deque.to_list |> List.reduce_exn ~f:Int.( + )
  ;;

  let%expect_test _ =
    let schematic =
      {|467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..|}
      |> Schematic.of_string
    in
    let answer = solve schematic in
    print_s [%message (answer : int)];
    [%expect {| (answer 467835) |}]
  ;;
end

let run which =
  match which with
  | `A ->
    let schematic = read_schematic () in
    let answer = A.solve schematic in
    print_s [%message (answer : int)]
  | `B ->
    let schematic = read_schematic () in
    let answer = B.solve schematic in
    print_s [%message (answer : int)]
;;
