let enable_additional_knapsack_tests = true

type behavior = Nice | Naughty
type notes = (string * behavior) list
type selection_alg = (string * int * int) list -> int -> string list

exception Invalid_file_format of string

let rec read_kv_file value_mapper filename =
  let file = open_in filename in
  let rec read acc =
    try
      let line = input_line file in
      match String.split_on_char ':' line with
      | ["";_] | [_;""] -> raise (Invalid_file_format filename)
      | [key;value] -> let v = value_mapper value in read ((key,v)::acc)
      | _ -> raise (Invalid_file_format filename)
    with End_of_file -> acc
  in
  try
    let r = read [] in
    close_in file;
    r
  with e -> close_in file; raise (Invalid_file_format filename)

(* 9.3 - 1 *)
let read_notes filename = read_kv_file
  (function "nice" -> Nice | "naughty" -> Naughty | _ -> raise (Invalid_file_format filename)) filename

(* 9.3 - 2 *)
let read_wishlist filename = read_kv_file
  (fun value -> let v = int_of_string value in if v < 1 || v > 100 then raise (Invalid_file_format filename) else v) filename

(* 9.3 - 3 *)
let load_catalogue filename = read_kv_file
  (fun value -> let v = int_of_string value in if v < 0 then raise (Invalid_file_format filename) else v) filename

(* 9.3 - 4 *)
let write_list filename presents =
  let file = open_out filename in
  let rec write = function [] -> ()
    | p::ps -> Printf.fprintf file "%s\n" p; write ps
  in
  write presents;
  close_out file

(* 9.3 - 5 *)
let write_letter filename =
  let file = open_out filename in
  output_string file "Sry, nothing for you, naughty one!";
  close_out file

(* 9.3 - 6 *)
let run_santas_factory (maxweight : int) (ks : selection_alg) : unit =
  let catalogue = load_catalogue "toys_catalogue.txt" in
  let pack_presents kid =
    try
      let wishes = read_wishlist (kid ^ "_wishlist.txt") in
      let wishes = List.map (fun (name,value) -> match List.assoc_opt name catalogue with None -> None
        | Some weight -> Some (name,value,weight)) wishes in
      let wishes = List.filter (fun w -> w <> None) wishes in
      let wishes = List.map (function Some (name,value,weight) -> name,value,weight | _ -> failwith "unreachable") wishes in
      let presents = ks wishes maxweight in
      write_list (kid ^ "_presents.txt") presents
    with _ -> ()
  in
  let notes = read_notes "santas_notes.txt" in
  let nice_kids, naughty_kids = List.partition (fun (_,b) -> b = Nice) notes in
  List.iter (fun (name,_) -> write_letter (name ^ "_letter.txt")) naughty_kids;
  List.iter (fun (name,_) -> pack_presents name) nice_kids

(* 9.3 - 7 *)
                       (* name * val * weight *)
let knapsack (items : (string * int * int) list) (maxweight : int) : string list =
  let items = List.sort (fun (_,_,w1) (_,_,w2) -> compare w1 w2) items in
  let first_row = List.init (maxweight + 1) (fun _ -> 0) in
  let rec build_row rows (iname, iv, iw) =
    let next_row = List.init (maxweight + 1) (fun w -> let prevv = List.nth (List.hd rows) w in (if w < iw then prevv else
      let newv = iv + (List.nth (List.hd rows) (w-iw)) in max prevv newv)) in next_row::rows
  in
  let matrix = List.fold_left build_row [first_row] items in
  let last_col = List.map (fun row -> List.nth row maxweight) matrix in
  let _,highest_row,_ = List.fold_left (fun (v,i,c) x -> if x > v then x,c,c-1 else v,i,c-1) (0,0,List.length items) last_col in
  let rec remove_unused_rows r mat =
    if r > highest_row then remove_unused_rows (r-1) (List.tl mat) else mat
  in
  let matrix = remove_unused_rows (List.length items) matrix in
  let sel,_,_ = List.fold_right (fun (iname,iv,iw) (sel,mat,remw) -> match mat with
    | cur::prev::rest -> if (List.nth cur remw) <> (List.nth prev remw) then (iname::sel,prev::rest,remw-iw) else (sel,prev::rest,remw)
    | _ -> failwith "unreachable") items ([],matrix,maxweight) in
  sel


(*****************************************************************************)
(**************************** END OF HOMEWORK ********************************)
(*****************************************************************************)
(* example inputs, you may use them to test your implementations,
   but [do not change] *)
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
  run_santas_factory 13 test_selection_alg;
  if not (check_letter "marta_letter.txt") then raise (Failure "no correct letter produced for marta");
  if not (check_letter "bruno_letter.txt") then raise (Failure "no correct letter produced for bruno");
  if not (check_file "frida_presents.txt" ["colorful pencils";"ocaml book"]) then raise (Failure "no correct present list produced for frida");
  if not (check_file "tommy_presents.txt" ["sand toys";"superman action doll set"]) then raise (Failure "no correct present list produced for tommy");
  if not (check_file "caren_presents.txt" ["penguin doll";"unicorn"]) then raise (Failure "no correct present list produced for caren");
  true


let (=^) a b =
    (List.sort compare a) = (List.sort compare b)

(*
let rec random_ks_input seed size =
  Random.init seed;
  List.init size (fun i -> string_of_int i, Random.int 100, Random.int 10)

let print_toy_list l =
  print_endline ("[" ^ String.concat ";" (List.map (fun (n,v,w) -> Printf.sprintf "(\"%s\",%d,%d)" n v w) l) ^ "]")

let print_selection l =
  print_endline ("[" ^ (String.concat ";" (List.map (fun s -> "\"" ^ s ^ "\"") l) ^ "]"))
*)

let a937_ex0 = [("0",24,7);("1",50,6);("2",37,6);("3",19,1);("4",32,2);("5",71,8);("6",54,2);("7",87,8);("8",9,6);("9",80,9);("10",21,9);("11",93,0);("12",5,4);("13",97,5);("14",34,8);("15",42,7);("16",11,1);("17",71,4);("18",1,8);("19",39,0)]
let a937_ex0_result = ["11";"19";"3";"6";"17";"13";"7"]
let a937_ex1 = [("0",16,6);("1",65,4);("2",26,0);("3",67,4);("4",93,9);("5",85,0);("6",2,6);("7",6,1);("8",76,4);("9",30,1);("10",50,1);("11",50,4);("12",53,5);("13",19,9);("14",1,3)]
let a937_ex1_result = ["2";"5";"7";"9";"10";"1";"3";"8";"12"]
let a937_ex2 = [("0",30,1);("1",31,8);("2",64,8);("3",54,6);("4",71,2);("5",44,9);("6",22,5);("7",67,5);("8",55,2);("9",24,8);("10",80,7);("11",40,5);("12",68,3);("13",90,7);("14",71,1);("15",43,3);("16",62,5);("17",40,8);("18",54,2);("19",27,1)]
let a937_ex2_result = ["0";"14";"19";"4";"8";"18";"12";"15";"7"]
let a937_ex3 = [("0",59,8);("1",45,7);("2",29,9);("3",43,3);("4",64,1);("5",71,2);("6",8,8);("7",43,4);("8",4,1);("9",22,8);("10",52,3);("11",60,1);("12",69,3);("13",12,2);("14",99,1);("15",94,3);("16",76,2);("17",68,8);("18",83,9);("19",29,3);("20",56,6);("21",21,5);("22",53,2);("23",86,1);("24",81,4)]
let a937_ex3_result = ["4";"11";"14";"23";"5";"16";"22";"12";"15";"24"]


let a933_ex1 = ["penguin doll",1; "ocaml book",2; "time machine",53; "bike",7; "barbie's dream house",5;
  "guitar",6; "colorful pencils",2; "socks",1; "shawl",2; "karaoke machine",13; "superman action doll set",3;
  "guinea pig",3; "horse",10; "unicorn",8; "sand toys",4; "soccer shoes",3]

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


(*****************************************************************************)
(* TESTS [do not change] *)
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









