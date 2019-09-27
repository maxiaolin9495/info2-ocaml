open Thread
open Event


(* 13.1 *)
let spawn_counter n = failwith "todo"

let run_counters m n = failwith "todo"


(* 13.2 *)
type blog = string list
type user = string
type pass = string
type message = Post of user * pass * string
  | Read of user * blog channel
type t = message channel

(* begin solution *)
let start_server users = failwith "todo"

let post s u p t = failwith "tood"

let read s u = failwith "todo"

(* end solution *)
let test =
  let s = start_server [("userA", "passA"); ("userB", "passB")] in
  post s "userB" "passB" "Welcome to my OCaml blog.";
  post s "userA" "passA" "My name is A and I'm starting my own blog!";
  post s "userB" "12345" "I am a hacker attacking B's blog now!";
  post s "userB" "passB" "You can have threads in OCaml!";
  read s "userB"


(* 13.3 *)
module Future = struct
  type 'a t = unit (* todo *)

  let create f a = failwith "todo"

  let get c = failwith "todo"

  let then_ f c = failwith "todo"

  let when_any cs = failwith "todo"

  let when_all cs = failwith "todo"

end



(* Future example *)
(*
let read_lines filename =
  let file = open_in filename in
  let rec read_all l =
    try
      read_all (input_line file :: l)
    with End_of_file -> List.rev l
  in
  let content = read_all [] in
  close_in file;
  content

let write_file filename content =
  let file = open_out filename in
  output_string file content;
  close_out file

let print_list l =
  print_endline (String.concat "\n" l)

let main () =
  let f1 = Future.create read_lines "p13.ml" in
  let f1 = Future.then_ (List.filter ((<>) "")) f1 in
  let f2 = Future.create read_lines "p13_sol.ml" in
  let f2 = Future.then_ (List.filter ((<>) "")) f2 in
(* let fany = Future.when_any [f1;f2] in
  print_list (Future.get fany)
  *)
  let fmerged = Future.when_all [f1;f2]
    |> Future.then_ (List.fold_left (@) [])
    |> Future.then_ (String.concat "\n")
    |> Future.then_ (write_file "merged.ml") in
  try Future.get fmerged with e -> print_endline "Exception!"
*)