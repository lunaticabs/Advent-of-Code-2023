let rec read_lines () =
  let line = read_line () in
  match line with
  | "" -> []  (* 输入为空行时停止读取 *)
  | _ -> line :: read_lines ()  (* 将非空行添加到列表中，并递归读取下一行 *)

let rec gets list_of_str = (*->list of listed str*)
  match list_of_str with
  | [] -> []
  | hd :: tl -> List.init (String.length hd) (String.get hd) :: gets tl

let is_number char =
  ((int_of_char char) - (int_of_char '0')) >= 0 &&
  ((int_of_char char) - (int_of_char '0')) <= 9

let num_filter listed_char =
  List.filter (fun x -> is_number x) listed_char

let rec list_of_char_to_int listed_num =
  match listed_num with
  | [] -> []
  | hd :: tl -> ((int_of_char hd) - (int_of_char '0')) :: list_of_char_to_int tl

let rec get_first_and_last lst =
  match lst with
  | [] -> None (* 空列表，返回 None *)
  | [x] -> Some [x; x] (* 只有一个元素，返回 (x, x) *)
  | first :: rest -> (* 至少有两个元素 *)
    let rec last_element = function
      | x :: [] -> x
      | _ :: tail -> last_element tail
      | [] -> failwith "Empty list"
    in
    Some [first; last_element rest]

let calculate lst =
  match lst with
  | Some [x; y] -> 
   (* print_int (x * 10 + y);
    print_newline (); *)
    x * 10 + y
  | _ -> failwith "too much or too few arguements in first and last"

let rec sum list_of_listed_str =
  match list_of_listed_str with
  | [] -> 0
  | hd :: tl -> 
    let current_sum = hd |> num_filter |> list_of_char_to_int |> get_first_and_last |> calculate in
    current_sum + sum tl

let _ = () |> read_lines |> gets |> sum |> print_int
