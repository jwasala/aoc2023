open Core
open Str

type cube_set = { red : int; green : int; blue : int }
type game = { id : int; sets : cube_set list }

let parse_set s =
  let items =
    s
    |> Str.split (regexp ",")
    |> List.map ~f:String.strip
    |> List.map ~f:(Str.split (regexp " "))
  in
  let empty = { red = 0; green = 0; blue = 0 } in
  List.fold_left items ~init:empty ~f:(fun { red; green; blue } item ->
      match item with
      | [ count; "red" ] -> { red = int_of_string count; green; blue }
      | [ count; "green" ] -> { red; green = int_of_string count; blue }
      | [ count; "blue" ] -> { red; green; blue = int_of_string count }
      | _ -> { red; green; blue })

let parse_game s =
  match Str.split (regexp ":") s with
  | [ game; sets ] -> (
      match Str.split (regexp " ") game with
      | [ _; game_num ] ->
          {
            id = game_num |> String.strip |> int_of_string;
            sets = sets |> Str.split (regexp ";") |> List.map ~f:parse_set;
          }
      | _ -> failwith "Wrong game format")
  | _ -> failwith "Wrong game format"

(* Solution 1 *)
let is_game_possible ~game:{ sets; _ } ~set:{ red; green; blue } =
  let red_min =
    sets |> List.map ~f:(fun set -> set.red) |> List.fold_left ~init:0 ~f:max
  in
  let green_min =
    sets |> List.map ~f:(fun set -> set.green) |> List.fold_left ~init:0 ~f:max
  in
  let blue_min =
    sets |> List.map ~f:(fun set -> set.blue) |> List.fold_left ~init:0 ~f:max
  in
  red >= red_min && green >= green_min && blue >= blue_min
;;

"day02.txt" |> In_channel.read_lines |> List.map ~f:parse_game
|> List.fold_left ~init:0 ~f:(fun acc game ->
       if is_game_possible ~game ~set:{ red = 12; green = 13; blue = 14 } then
         acc + game.id
       else acc)
|> Out_channel.printf "%d"
;;

(* Solution 2 *)
Out_channel.printf "\n"

let get_game_power { sets; _ } =
  let red =
    sets |> List.map ~f:(fun set -> set.red) |> List.fold_left ~init:0 ~f:max
  in
  let green =
    sets |> List.map ~f:(fun set -> set.green) |> List.fold_left ~init:0 ~f:max
  in
  let blue =
    sets |> List.map ~f:(fun set -> set.blue) |> List.fold_left ~init:0 ~f:max
  in
  red * green * blue
;;

"day02.txt" |> In_channel.read_lines |> List.map ~f:parse_game
|> List.map ~f:get_game_power |> List.reduce ~f:( + )
|> fun x ->
match x with
| Some x -> Out_channel.printf "%d" x
| _ -> Out_channel.printf "No games found"
