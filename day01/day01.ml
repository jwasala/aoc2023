open Core
open Str

let rec find_first = function
  | h :: _ when Char.is_digit h -> int_of_string @@ Char.escaped h
  | _ :: t -> find_first t
  | [] -> failwith "Digit not found"

let decipher (a, b) = (a * 10) + b

let mappings =
  [
    ("one", "1");
    ("two", "2");
    ("three", "3");
    ("four", "4");
    ("five", "5");
    ("six", "6");
    ("seven", "7");
    ("eight", "8");
    ("nine", "9");
  ]

let mappings_rev =
  mappings |> List.map ~f:(fun (num1, num2) -> (num1 |> String.rev, num2))

let rec replace_numbers ~mappings s =
  match s with
  | "" -> ""
  | s ->
      let replaced =
        List.fold_left mappings ~init:s ~f:(fun s (num1, num2) ->
            if Stdlib.String.starts_with ~prefix:num1 s then
              Str.replace_first (Str.regexp num1) num2 s
            else s)
      in
      Str.string_before replaced 1 ^ Str.string_after replaced 1
;;

(* Solution 1 *)
"day01.txt" |> In_channel.read_lines
|> List.map ~f:(fun xs ->
       ( xs |> String.to_list |> find_first,
         xs |> String.to_list_rev |> find_first ))
|> List.map ~f:decipher
|> List.fold_left ~f:( + ) ~init:0
|> Out_channel.printf "%d"
;;

Out_channel.printf "\n";;

(* Solution 2 *)
"day01.txt" |> In_channel.read_lines
|> List.map ~f:(fun xs ->
       ( xs |> replace_numbers ~mappings |> String.to_list |> find_first,
         xs |> String.rev
         |> replace_numbers ~mappings:mappings_rev
         |> String.to_list |> find_first ))
|> List.map ~f:decipher
|> List.fold_left ~f:( + ) ~init:0
|> Out_channel.printf "%d"
