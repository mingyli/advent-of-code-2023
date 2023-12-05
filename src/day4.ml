open! Core
open More

module Card = struct
  type t =
    { id : int
    ; winning_numbers : int list
    ; numbers_you_have : int list
    }
  [@@deriving sexp, fields]

  let of_string s =
    match String.split_on s ~on:":" with
    | [ card; numbers ] ->
      let id =
        match String.split_whitespace card with
        | [ "Card"; id ] -> Int.of_string id
        | _ -> assert false
      in
      (match String.split_on numbers ~on:"|" with
       | [ winning_numbers; numbers_you_have ] ->
         let winning_numbers =
           String.split_whitespace winning_numbers |> List.map ~f:Int.of_string
         and numbers_you_have =
           String.split_whitespace numbers_you_have |> List.map ~f:Int.of_string
         in
         { id; winning_numbers; numbers_you_have }
       | _ -> assert false)
    | _ -> assert false
  ;;

  let num_matches t =
    List.count t.numbers_you_have ~f:(fun i ->
      List.mem t.winning_numbers i ~equal:Int.equal)
  ;;

  let score t =
    let num_winners = num_matches t in
    match num_winners with
    | 0 -> 0
    | num_winners -> Int.pow 2 (num_winners - 1)
  ;;

  let%expect_test _ =
    let card = of_string {|Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53|} in
    print_s [%sexp (card : t)];
    [%expect
      {|
      ((id 1) (winning_numbers (41 48 83 86 17))
       (numbers_you_have (83 86 6 31 17 9 48 53))) |}]
  ;;
end

module A = struct
  let solve cards = List.sum (module Int) cards ~f:Card.score

  let%expect_test _ =
    let cards =
      {|Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11 |}
      |> String.split_lines
      |> List.map ~f:Card.of_string
    in
    let answer = solve cards in
    print_s [%message (answer : int)];
    [%expect {| (answer 13) |}]
  ;;
end

module B = struct
  type t = { num_copies : int Int.Table.t } [@@deriving sexp]

  let num_copies t card =
    Hashtbl.find_or_add t.num_copies (Card.id card) ~default:(Fn.const 1)
  ;;

  let add_copies t card_id num_copies =
    Hashtbl.update t.num_copies card_id ~f:(function
      | None -> 1 + num_copies
      | Some n -> n + num_copies)
  ;;

  let total_copies t = Hashtbl.data t.num_copies |> List.sum (module Int) ~f:Fn.id

  let solve cards =
    let t = { num_copies = Int.Table.create () } in
    List.iter cards ~f:(fun card ->
      let num_copies = num_copies t card in
      let num_matches = Card.num_matches card in
      List.range ~stop:`inclusive (card.id + 1) (card.id + num_matches)
      |> List.iter ~f:(fun id -> add_copies t id num_copies));
    total_copies t
  ;;

  let%expect_test _ =
    let cards =
      {|Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11 |}
      |> String.split_lines
      |> List.map ~f:Card.of_string
    in
    let answer = solve cards in
    print_s [%message (answer : int)];
    [%expect {|
      (answer 30) |}]
  ;;
end

let read_cards () = In_channel.(input_lines stdin) |> List.map ~f:Card.of_string

let run which =
  match which with
  | `A ->
    let cards = read_cards () in
    let answer = A.solve cards in
    print_s [%message (answer : int)]
  | `B ->
    let cards = read_cards () in
    let answer = B.solve cards in
    print_s [%message (answer : int)]
;;
