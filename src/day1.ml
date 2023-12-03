open! Core

let read_document () = In_channel.(input_lines stdin)

module A = struct
  let calibration line =
    let chars = String.to_array line in
    let digits = Array.filter chars ~f:Char.is_digit |> Array.map ~f:Char.get_digit_exn in
    let first = Array.get digits 0
    and last = Array.last digits in
    (10 * first) + last
  ;;

  let solve document =
    document |> List.map ~f:calibration |> List.sum (module Int) ~f:Fn.id
  ;;

  let%expect_test _ =
    let document =
      {|1abc2
  pqr3stu8vwx
  a1b2c3d4e5f
  treb7uchet|} |> String.split_lines
    in
    let answer = solve document in
    print_s [%message (answer : int)];
    [%expect {| (answer 142) |}]
  ;;
end

module B = struct
  let number_at_head s =
    let ch = String.get s 0 in
    match Char.get_digit ch with
    | Some digit -> Some digit
    | None ->
      let is_prefix prefix = String.is_prefix s ~prefix in
      if is_prefix "one"
      then Some 1
      else if is_prefix "two"
      then Some 2
      else if is_prefix "three"
      then Some 3
      else if is_prefix "four"
      then Some 4
      else if is_prefix "five"
      then Some 5
      else if is_prefix "six"
      then Some 6
      else if is_prefix "seven"
      then Some 7
      else if is_prefix "eight"
      then Some 8
      else if is_prefix "nine"
      then Some 9
      else None
  ;;

  let calibration line =
    let numbers =
      Sequence.unfold_step ~init:line ~f:(fun s ->
        match String.is_empty s with
        | true -> Done
        | false ->
          let next = String.drop_prefix s 1 in
          (match number_at_head s with
           | Some number -> Yield (number, next)
           | None -> Skip next))
    in
    let numbers = Sequence.to_list numbers in
    let first = List.hd_exn numbers
    and last = List.last_exn numbers in
    (10 * first) + last
  ;;

  let solve document =
    document |> List.map ~f:calibration |> List.sum (module Int) ~f:Fn.id
  ;;

  let%expect_test _ =
    let document =
      {|two1nine
      eightwothree
      abcone2threexyz
      xtwone3four
      4nineeightseven2
      zoneight234
      7pqrstsixteen|}
      |> String.split_lines
    in
    let answer = solve document in
    print_s [%message (answer : int)];
    [%expect {| (answer 281) |}]
  ;;
end

let run which =
  let document = read_document () in
  match which with
  | `A ->
    let answer = A.solve document in
    print_s [%message (answer : int)]
  | `B ->
    let answer = B.solve document in
    print_s [%message (answer : int)]
;;
