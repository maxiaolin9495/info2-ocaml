let enable_additional_knapsack_tests = false
let todo _ = failwith "TODO"

type behavior = Nice | Naughty
type notes = (string * behavior) list
type selection_alg = (string * int * int) list -> int -> string list

exception Invalid_file_format of string


(* 9.3 - 1 *)
let read_notes filename = 
  let file = open_in filename in
  let get_behavior behaviors = if behaviors = "nice"then Nice else Naughty in 
  let rec read_notes_help output = 
    try let line = input_line file in
    match String.split_on_char ':' line with
    |""::_ | _::""::_ -> raise (Invalid_file_format filename)
    | [names; behaviors] -> if names = "" then raise (Invalid_file_format filename)
      else if behaviors <> "nice" && behaviors <> "naughty" then raise (Invalid_file_format filename)
      else read_notes_help ((names, get_behavior behaviors)::output)
    |_ -> raise (Invalid_file_format filename)
    with End_of_file -> output
  in try 
    let a = read_notes_help [] |> List.rev in 
    close_in file;
    a
  with e -> close_in file; raise e

(* 9.3 - 2 *)
let read_wishlist filename = 
  let file = open_in filename in
  let rec read_notes_help output = 
    try let line = input_line file in
    match String.split_on_char ':' line with
    |""::_ | _::""::_ -> raise (Invalid_file_format filename)
    | [gift; importance] -> let importancenumber = try int_of_string importance with _ -> raise (Invalid_file_format filename) 
    in if gift = "" then raise (Invalid_file_format filename)
    else if importancenumber < 1 || importancenumber > 100 then raise (Invalid_file_format filename)
      else read_notes_help ((gift, importancenumber)::output)
    |_ -> raise (Invalid_file_format filename)
    with End_of_file -> output
  in try 
    let a = read_notes_help [] |> List.rev in 
    close_in file;
    a
  with e -> close_in file; raise e
(* 9.3 - 3 *)
let load_catalogue filename = 
  let file = open_in filename in
  let rec read_notes_help output = 
    try let line = input_line file in
    match String.split_on_char ':' line with
    |""::_ | _::""::_ -> raise (Invalid_file_format filename)
    | [toy; weights] -> let weight = try int_of_string weights with _ -> raise (Invalid_file_format filename) 
    in if toy = "" then raise (Invalid_file_format filename)
    else if weight < 1 then raise (Invalid_file_format filename)
      else read_notes_help ((toy, weight)::output)
    |_ -> raise (Invalid_file_format filename)
    with End_of_file -> output
  in try 
    let a = read_notes_help [] |> List.rev in 
    close_in file;
    a
  with e -> close_in file; raise e

(* 9.3 - 4 *)
let write_list filename l = let file = open_out filename in
  let write_present name = output_string file (name ^ "\n") in 
  List.iter write_present l;
  close_out file

(* 9.3 - 5 *)
let write_letter filename = let file = open_out filename in
  output_string file "Keep Naughty! \n No gifts come, only large black dogs! \n CF";
  close_out file



(* 9.3 - 6 *)

let rec check_child_behaviour l output = match l with (name, Naughty)::ls -> write_letter (name ^ "_letter.txt"); check_child_behaviour ls output
 | (name, Nice) :: ls -> check_child_behaviour ls (name::output)
 | []-> List.rev output

let rec find_weight name lists = match lists with (p, weight)::ls -> if name = p then weight else find_weight name ls
| [] -> 0

let rec help1 name l output = match l with (a,b)::ls-> help1 name ls ((name,a,b)::output)
  | [] -> output

let create_presents_lists name = write_list (name ^ "_presents.txt") []

let rec read_string_line filename s output = let file = open_in filename in
  let rec read_help output = 
  try let line = input_line file in 
  read_help (line::output)
  with End_of_file -> List.rev (s::output)
  | e -> raise e in 
  let output = read_help [] in 
  close_in file;
  output

let run_santas_factory capacity f = let cata_list = load_catalogue "examples/toys_catalogue.txt" in 
  let children_list = read_notes "examples/santas_notes.txt" in
  let nice_children = check_child_behaviour children_list [] in
  let rec get_presents_list output (p1, importance_number) = let weight = find_weight p1 cata_list in if weight = 0 then output else (p1, importance_number, weight)::output in
  let find_and_insert name = 
    let wishlist = read_wishlist (name^"_wishlist.txt") in 
    let pre_selection_list = List.fold_left get_presents_list [] wishlist in
    let selection_list = f pre_selection_list capacity in let find_help p = let find_help_help present = 
    if p = present then write_list (name^"_presents.txt") (read_string_line (name^"_presents.txt") p []) in 
    List.iter find_help_help selection_list in 
    List.iter find_help selection_list in
  List.iter create_presents_lists nice_children;
  List.iter find_and_insert nice_children


(*9.3 - 7 *)
let knapsack pre_selection_list capacity=  
  let get_importance_to_weight_rate  importance weight = float_of_int importance /. float_of_int weight in
  let rec remove_present pre l output = match l with (present,a,b)::xs -> if pre = present then remove_present "" xs output else remove_present pre xs ((present, a ,b)::output)
    | [] -> List.rev output in
  let rec get_most_important l (p,i,w) r = match l with (present, importance, weight)::ls -> let rate = get_importance_to_weight_rate importance weight in 
      if rate > r then get_most_important ls (present, importance, weight) rate else get_most_important ls (p,i,w)r 
    | [] -> (p,i,w) in

  let rec knapsack_help l capacity left_capacity output = 
    let (p,i,w) = get_most_important l ("", 0, 0) 0.0 in 
    let l1 = remove_present p l [] in
    if l = [] then output else 
      if w > left_capacity then knapsack_help l1 capacity left_capacity output
      else knapsack_help l1 capacity (left_capacity-w) (p::output) in
  knapsack_help pre_selection_list capacity capacity []  



(*****************************************************************************)
(**************************** END OF HOMEWORK ********************************)
(*****************************************************************************)
(* example inputs, you may use them to test your implementations,
   but [do not change] *)
let a933_ex1 = ["penguin doll",1; "ocaml book",2; "time machine",53; "bike",7; "barbie's dream house",5;
  "guitar",6; "colorful pencils",2; "socks",1; "shawl",2; "karaoke machine",13; "superman action doll set",3;
  "guinea pig",3; "horse",10; "unicorn",8; "sand toys",4; "soccer shoes",3]

let a937_ex0 = [("0",24,7);("1",50,6);("2",37,6);("3",19,1);("4",32,2);("5",71,8);("6",54,2);("7",87,8);("8",9,6);("9",80,9);("10",21,9);("11",93,0);("12",5,4);("13",97,5);("14",34,8);("15",42,7);("16",11,1);("17",71,4);("18",1,8);("19",39,0)]
let a937_ex0_result = ["11";"19";"3";"6";"17";"13";"7"]
let a937_ex1 = [("0",16,6);("1",65,4);("2",26,0);("3",67,4);("4",93,9);("5",85,0);("6",2,6);("7",6,1);("8",76,4);("9",30,1);("10",50,1);("11",50,4);("12",53,5);("13",19,9);("14",1,3)]
let a937_ex1_result = ["2";"5";"7";"9";"10";"1";"3";"8";"12"]
let a937_ex2 = [("0",30,1);("1",31,8);("2",64,8);("3",54,6);("4",71,2);("5",44,9);("6",22,5);("7",67,5);("8",55,2);("9",24,8);("10",80,7);("11",40,5);("12",68,3);("13",90,7);("14",71,1);("15",43,3);("16",62,5);("17",40,8);("18",54,2);("19",27,1)]
let a937_ex2_result = ["0";"14";"19";"4";"8";"18";"12";"15";"7"]
let a937_ex3 = [("0",59,8);("1",45,7);("2",29,9);("3",43,3);("4",64,1);("5",71,2);("6",8,8);("7",43,4);("8",4,1);("9",22,8);("10",52,3);("11",60,1);("12",69,3);("13",12,2);("14",99,1);("15",94,3);("16",76,2);("17",68,8);("18",83,9);("19",29,3);("20",56,6);("21",21,5);("22",53,2);("23",86,1);("24",81,4)]
let a937_ex3_result = ["4";"11";"14";"23";"5";"16";"22";"12";"15";"24"]


(*****************************************************************************)
(* TESTS [do not change] *)
let (=^) a b =
    (List.sort compare a) = (List.sort compare b)
let (=|) a b =
    let a = List.sort_uniq (fun x y -> compare (fst x) (fst y)) a in
    let b = List.sort_uniq (fun x y -> compare (fst x) (fst y)) b in
    a = b
let check_throws e f =
  try f (); false with e' -> e' = e

let check_file filename content =
  let file = open_in filename in
  let rec read acc =
    try
      read ((input_line file)::acc)
    with End_of_file -> acc
  in
  let c = read [] in
  close_in file;
  (List.sort_uniq compare c) = (List.sort_uniq compare content)

let check_letter filename =
  let file = open_in filename in
  let rec read () =
    try
      let line = input_line file in
      if line <> "" then true else
      read ()
    with End_of_file -> false
  in
  let r = read () in
  close_in file;
  r

let raise' = function Failure f ->
  Printf.printf "TEST FAILURE: %s\n" f;
  raise (Failure f)
  | e -> raise e

let check_run_santas_factory () =
  let test_selection_alg wishes capacity =
    if capacity <> 13 then raise' (Failure "wrong capacity passed to selection_alg");
    (match List.find_opt (fun (t,_,_) -> t = "ocaml book") wishes with
    | None -> raise' (Failure "wrong list passed to selection_alg")
    | Some (_,_,w) -> if w <> 2 then raise' (Failure "wrong list passed to selection_alg"));
    match List.sort (fun (_,i,_) (_,j,_) -> compare j i) wishes with
    | (w1,_,_)::(w2,_,_)::_ -> [w1;w2]
    | _ -> raise' (Failure "wrong list passed to selection_alg")
  in
  ignore(run_santas_factory 13 test_selection_alg);
  if not (check_letter "marta_letter.txt") then raise (Failure "no correct letter produced for marta");
  if not (check_letter "bruno_letter.txt") then raise (Failure "no correct letter produced for bruno");
  if not (check_file "frida_presents.txt" ["colorful pencils";"ocaml book"]) then raise (Failure "no correct present list produced for frida");
  if not (check_file "tommy_presents.txt" ["sand toys";"superman action doll set"]) then raise (Failure "no correct present list produced for tommy");
  if not (check_file "caren_presents.txt" ["penguin doll";"unicorn"]) then raise (Failure "no correct present list produced for caren");
  true

let ks_a937_ex1_rt : float option ref = ref None
let ks_a937_ex2_rt : float option ref = ref None
let ks_a937_ex3_rt : float option ref = ref None
let check_is_knapsack_efficient () =
  match !ks_a937_ex1_rt, !ks_a937_ex2_rt, !ks_a937_ex3_rt with
  | Some a, Some b, Some c -> (* Printf.printf "a = %f, b = %f, c = %f, quotients: %.2f | %.2f\n" a b c (b /. a) (c /. b); *) (b /. a) <= 5. && (c /. b) <= 5.
  | _ -> false

let measure_ks_runtime alg input size =
    let start = Sys.time () in
    let result = alg input size in
    result, Sys.time () -. start


let tests = [
  (* tests for 9.3 - 1 *)
  __LINE_OF__ (fun () -> (read_notes "examples/santas_notes.txt") =| ["tommy",Nice;"bruno",Naughty;"frida",Nice;"caren",Nice;"marta",Naughty]);
  __LINE_OF__ (fun () -> let fn = "examples/santas_notes_broken1.txt" in check_throws (Invalid_file_format fn) (fun () -> read_notes fn));
  __LINE_OF__ (fun () -> let fn = "examples/santas_notes_broken2.txt" in check_throws (Invalid_file_format fn) (fun () -> read_notes fn));
  (* tests for 9.3 - 2 *)
  __LINE_OF__ (fun () -> (read_wishlist "examples/frida_wishlist.txt") =| ["ocaml book",10;"horse",3;"colorful pencils",12]);
  __LINE_OF__ (fun () -> let fn = "examples/wishlist_broken1.txt" in check_throws (Invalid_file_format fn) (fun () -> read_wishlist fn));
  __LINE_OF__ (fun () -> let fn = "examples/wishlist_broken2.txt" in check_throws (Invalid_file_format fn) (fun () -> read_wishlist fn));
  __LINE_OF__ (fun () -> let fn = "examples/wishlist_broken3.txt" in check_throws (Invalid_file_format fn) (fun () -> read_wishlist fn));
  (* tests for 9.3 - 3 *)
  __LINE_OF__ (fun () -> (load_catalogue "examples/toys_catalogue.txt") =| a933_ex1);
  __LINE_OF__ (fun () -> let fn = "examples/toys_catalogue_broken1.txt" in check_throws (Invalid_file_format fn) (fun () -> load_catalogue fn));
  __LINE_OF__ (fun () -> let fn = "examples/toys_catalogue_broken2.txt" in check_throws (Invalid_file_format fn) (fun () -> load_catalogue fn));
  __LINE_OF__ (fun () -> let fn = "examples/toys_catalogue_broken3.txt" in check_throws (Invalid_file_format fn) (fun () -> load_catalogue fn));
  (* tests for 9.3 - 4 *)
  __LINE_OF__ (fun () -> let l = ["socks";"colorful pencils";"horse"] in let fn = "examples/testout_list1.txt" in write_list fn l; check_file fn l);
  (* tests for 9.3 - 5 *)
  __LINE_OF__ (fun () -> let fn = "examples/testout_letter1.txt" in write_letter fn; check_letter fn);
  (* tests for 9.3 - 6 *)
  __LINE_OF__ (fun () -> check_run_santas_factory ());
  (* tests for 9.3 - 7 *)
  __LINE_OF__ (fun () -> (knapsack ["a",5,4; "b",2,2; "b",2,2; "d",4,5; "b",2,2; "e",8,2] 10) =^ ["a";"b";"b";"e"]);
  __LINE_OF__ (fun () -> (knapsack ["a",5,4; "a",5,4; "c",11,6; "d",4,5; "e",8,2; "a",5,4] 10) =^ ["c";"e"]);
] @ if enable_additional_knapsack_tests then [
  (* additional tests *)
  __LINE_OF__ (fun () -> (knapsack ["a",100,10; "b",81,9; "c",81,9] 18) =^ ["b";"c"]); (* correctness check 0a *)
  __LINE_OF__ (fun () -> (knapsack a937_ex0 20) =^ a937_ex0_result); (* correctness check 0b *)
  __LINE_OF__ (fun () -> let result, rt = measure_ks_runtime knapsack a937_ex1 20 in ks_a937_ex1_rt := Some rt; result =^ a937_ex1_result); (* correctness check 1 *)
  __LINE_OF__ (fun () -> let result, rt = measure_ks_runtime knapsack a937_ex2 20 in ks_a937_ex2_rt := Some rt; result =^ a937_ex2_result); (* correctness check 2 *)
  __LINE_OF__ (fun () -> let result, rt = measure_ks_runtime knapsack a937_ex3 20 in ks_a937_ex3_rt := Some rt; result =^ a937_ex3_result); (* correctness check 3 *)
  __LINE_OF__ (fun () -> check_is_knapsack_efficient ());
] else []

let () =
  let rec input_lines ch =
    (try Some (input_line ch) with _ -> None) (* catch stupid EOF exception *)
    |> function Some line -> line :: input_lines ch | None -> []
  in
  let lines = input_lines (open_in __FILE__) in
  let open List in
  let open Printf in
  let fail l =
    let line = nth lines (l-1) in
    let test = String.sub line 25 (String.length line - 27) in
    printf "test \027[31;m%s\027[0;m (line %d) failed!\n" test l;
  in
  let test (l, t) =
    let ok = try t () with e -> print_endline (Printexc.to_string e); false in
    if not ok then fail l;
    ok
  in
  let passed = filter (fun x -> x) (map test tests) in
  printf "passed %d/%d tests\n" (length passed) (length tests)









