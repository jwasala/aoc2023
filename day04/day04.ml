open Core
open Str

type card = { id : int; winning_nums : int list; nums : int list }

let parse_nums x =
  x
  |> split (regexp " +")
  |> List.map ~f:(fun x -> x |> Stdlib.String.trim |> int_of_string)

let parse_card s =
  match split (regexp ":") s with
  | [ card_part; total_nums_part ] -> (
      match split (regexp " +") card_part with
      | [ _; card_num ] -> (
          match split (regexp "|") total_nums_part with
          | [ winning_nums; nums ] ->
              {
                id = int_of_string card_num;
                winning_nums = parse_nums winning_nums;
                nums = parse_nums nums;
              }
          | _ -> assert false)
      | _ -> assert false)
  | _ -> assert false

let count_card_matches c =
  List.count c.nums ~f:(fun x -> List.exists c.winning_nums ~f:(fun y -> x = y))

(* Solution 1 *)
let count_card_score c =
  let matching = count_card_matches c in
  if matching = 0 then 0 else Int.pow 2 (matching - 1)
;;

"day04.txt" |> In_channel.read_lines |> List.map ~f:parse_card
|> List.map ~f:count_card_score
|> List.fold_left ~init:0 ~f:( + )
|> Out_channel.printf "%d"
;;

(* Solution 2 *)
Out_channel.printf "\n"

let original_cards =
  "day04.txt" |> In_channel.read_lines |> List.map ~f:parse_card

let rec process_cards deck ~total =
  match deck with
  | [] -> total
  | h :: t ->
      let card_score = count_card_matches h in
      let extra_card_numbers = List.init card_score ~f:(fun x -> x + h.id) in
      let extra_cards =
        List.map extra_card_numbers ~f:(fun num ->
            List.nth_exn original_cards num)
      in
      process_cards (extra_cards @ t) ~total:(total + 1)
;;

process_cards original_cards ~total:0 |> Out_channel.printf "%d"
