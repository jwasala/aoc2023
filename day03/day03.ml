open Core

type coord = int * int
type part_number = { num : int; start : coord; stop : coord }
type symbol = { _c : char; pos : coord }

let make_part_number (row, col, num) =
  {
    num = int_of_string num;
    start = (row, col - String.length num);
    stop = (row, col - 1);
  }

let find_numbers_in_row xs row =
  let rec aux xs (row, col, num, nums) =
    match (xs, num) with
    | [], "" -> nums
    | [], _ -> make_part_number (row, col, num) :: nums
    | h :: t, _ when Char.is_digit h ->
        aux t (row, col + 1, num ^ Char.to_string h, nums)
    | _ :: t, "" -> aux t (row, col + 1, "", nums)
    | _ :: t, _ ->
        aux t (row, col + 1, "", make_part_number (row, col, num) :: nums)
  in
  aux xs (row, 0, "", [])

let find_numbers xss =
  Stdlib.List.flatten
  @@ List.foldi xss ~init:[] ~f:(fun i acc xs ->
         find_numbers_in_row xs i :: acc)

let find_symbols_in_row xs row =
  List.foldi xs ~init:[] ~f:(fun i acc x ->
      if Char.is_digit x || Char.equal '.' x then acc
      else { _c = x; pos = (row, i) } :: acc)

let find_symbols xss =
  Stdlib.List.flatten
  @@ List.foldi xss ~init:[] ~f:(fun i acc xs ->
         find_symbols_in_row xs i :: acc)

(* Solution 1 *)
let is_number_adjacent { start; stop; _ } symbols =
  let xf, yf = start in
  let _, yt = stop in
  List.exists symbols ~f:(fun s ->
      let xs, ys = s.pos in
      (xf = xs && yt = ys - 1)
      || (xf = xs && yf = ys + 1)
      || (abs (xf - xs) = 1 && ys >= yf - 1 && ys <= yt + 1))

let sum_adjacent_numbers xss =
  let symbols = find_symbols xss in
  let numbers = find_numbers xss in
  List.fold_left numbers ~init:0 ~f:(fun sum num ->
      if is_number_adjacent num symbols then sum + num.num else sum)
;;

"day03.txt" |> In_channel.read_lines |> List.map ~f:String.to_list
|> sum_adjacent_numbers |> Out_channel.printf "%d"
;;

Out_channel.printf "\n"

(* Solution 2 *)
let get_gear_value { _c; pos } numbers =
  if not @@ Char.equal _c '*' then 0
  else
    let xs, ys = pos in
    let gear =
      List.filter numbers ~f:(fun num ->
          let xf, yf = num.start in
          let _, yt = num.stop in
          (xf = xs && yt = ys - 1)
          || (xf = xs && yf = ys + 1)
          || (abs (xf - xs) = 1 && ys >= yf - 1 && ys <= yt + 1))
    in
    if List.length gear = 2 then
      List.fold gear ~init:1 ~f:(fun acc g -> acc * g.num)
    else 0

let get_gear_total xss =
  let symbols = find_symbols xss in
  let numbers = find_numbers xss in
  List.fold_left symbols ~init:0 ~f:(fun sum sym ->
      sum + get_gear_value sym numbers)
;;

"day03.txt" |> In_channel.read_lines |> List.map ~f:String.to_list
|> get_gear_total |> Out_channel.printf "%d"
