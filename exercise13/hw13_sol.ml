(* testing utilities [do not change] *)
let todo _ = failwith "TODO"

exception SyncDeadlocked
module Event = struct
  include Event

  let tsync t e =
    let timer = new_channel () in
    let run_timer () =
      Thread.delay t;
      poll (send timer None)
    in
    let _ = Thread.create run_timer () in
    match (select [wrap e (fun x -> Some x); receive timer]) with
    | Some x -> x
    | None -> raise SyncDeadlocked

  let tselect t es =
    tsync t (choose es)

  let sync e = tsync 2. e
  let select es = tselect 2. es
end

module Thread = struct
  include Thread

  let tc = ref 0

  let create f a =
    tc := !tc + 1;
    create f a
end


(*****************************************************************************)
(*************************** START OF HOMEWORK *******************************)
(*****************************************************************************)
open Thread
open Event

(* 13.4 *)
let par_unary f a =
  let worker a =
    let c = new_channel () in
    let _ = create (fun () -> sync (send c (f a))) () in
    c
  in
  List.map worker a |> List.map (fun c -> sync (receive c))


let par_binary f a b =
  let worker a b =
    let c = new_channel () in
    let _ = create (fun () -> sync (send c (f a b))) () in
    c
  in
  List.map2 worker a b |> List.map (fun c -> sync (receive c))


(* 13.5 *)
exception OutOfBounds

module Array = struct
  type 'a msg = Size | Get of int | Set of int * 'a | Answer of 'a | Ok of int | Error of exn | Resize of int * 'a | Destroy
  type 'a t = 'a msg channel

  let make s v =
    let rec update i v = function
      | [] -> []
      | x::xs -> if i = 0 then v::xs else x::update (i-1) v xs
    in
    let rec resize s v = function
      | [] -> List.init s (fun _ -> v)
      | x::xs -> if s <= 0 then [] else x::resize (s-1) v xs
    in
    let c = new_channel () in
    let rec loop entries =
      match sync (receive c) with
      | Size -> sync (send c (Ok (List.length entries))); loop entries
      | Get i -> sync (send c (try Answer (List.nth entries i) with _ -> Error OutOfBounds)); loop entries
      | Set (i, v) ->
        if i < 0 || (List.length entries) <= i
        then (sync (send c (Error OutOfBounds)); loop entries)
        else sync (send c (Ok 0)); loop (update i v entries)
      | Resize (s, v) -> loop (resize s v entries)
      | Destroy -> ()
      | _ -> failwith "unreachable"
    in
    let _ = create loop [] in
    sync (send c (Resize (s, v)));
    c

  let size a =
    sync (send a Size);
    match sync (receive a) with Ok x -> x | Error e -> raise e | _ -> failwith "unreachable"

  let set i v a =
    sync (send a (Set (i, v)));
    match sync (receive a) with Ok _ -> () | Error e -> raise e | _ -> failwith "unreachable"

  let get i a =
    sync (send a (Get i));
    match sync (receive a) with Answer x -> x | Error e -> raise e | _ -> failwith "unreachable"

  let resize s v a =
    sync (send a (Resize (s, v)))

  let destroy a =
    sync (send a Destroy)

end


(* 13.6 *)
type document = string
type username = string
type password = string

type entry = { id:int; doc:document; owner:username; viewers:username list }
type account = { user:username; pass:password }

type request = Publish of account * document * response channel
             | ChangeOwner of account * int * username * response channel
             | View of account * int * response channel
             | AddAccount of account * response channel
             | AddViewer of account * int * username * response channel
and response = Published of int | Viewed of string | Ok | Failed

exception InvalidOperation


let document_server () =
  let c = new_channel () in
  let rec loop accs docs next_id =
    let check_account a =
      List.find_opt ((=) a) accs <> None
    in
    match sync (receive c) with
    | Publish (acc, doc, answer_c) ->
      if not (check_account acc) then (sync (send answer_c Failed); loop accs docs next_id)
      else (sync (send answer_c (Published next_id));
            loop accs ({ id=next_id; doc=doc; owner=acc.user; viewers=[] }::docs) (next_id + 1))
    | ChangeOwner (acc, id, new_owner, answer_c) ->
      if not (check_account acc) then (sync (send answer_c Failed); loop accs docs next_id)
      else if List.find_opt (fun a -> a.user = new_owner) accs = None then (sync (send answer_c Failed); loop accs docs next_id)
      else if List.find_opt (fun d -> d.id = id && d.owner = acc.user) docs = None then (sync (send answer_c Failed); loop accs docs next_id)
      else (sync (send answer_c Ok);
            let new_docs = List.map (fun e -> if e.id = id then { e with owner=new_owner } else e) docs in
            loop accs new_docs next_id)
    | View (acc, id, answer_c) ->
      if not (check_account acc) then (sync (send answer_c Failed); loop accs docs next_id)
      else (match List.find_opt (fun e -> e.id = id) docs with
          | None -> sync (send answer_c Failed); loop accs docs next_id
          | Some e -> (if e.owner = acc.user || List.find_opt ((=) (acc.user)) e.viewers <> None then
                         sync (send answer_c (Viewed e.doc)) else sync (send answer_c Failed)); loop accs docs next_id)
    | AddViewer (acc, id, viewer, answer_c) ->
      if not (check_account acc) then (sync (send answer_c Failed); loop accs docs next_id)
      else (match List.find_opt (fun e -> e.id = id) docs with
          | None -> sync (send answer_c Failed); loop accs docs next_id
          | Some e-> if e.owner = acc.user then (sync (send answer_c Ok);
                                                 let new_docs = List.map (fun e -> if e.id = id then { e with viewers=viewer::e.viewers } else e) docs in
                                                 loop accs new_docs next_id))
    | AddAccount (acc, answer_c) ->
      if List.find_opt (fun a -> a.user = acc.user) accs <> None then (sync (send answer_c Failed); loop accs docs next_id)
      else (sync (send answer_c Ok); loop (acc::accs) docs next_id)
  in
  let _ = create (loop [] []) 0 in
  c

let publish u p doc s =
  let c = new_channel () in
  sync (send s (Publish ({ user=u; pass=p; }, doc, c)));
  match sync (receive c) with | Published id -> id | _ -> raise InvalidOperation

let change_owner u p id owner s =
  let c = new_channel () in
  sync (send s (ChangeOwner ({ user=u; pass=p }, id, owner, c)));
  match sync (receive c) with Ok -> () | _ -> raise InvalidOperation

let view u p id s =
  let c = new_channel () in
  sync (send s (View ({ user=u; pass=p }, id, c)));
  match sync (receive c) with Viewed d -> d | _ -> raise InvalidOperation

let add_account u p s =
  let c = new_channel () in
  sync (send s (AddAccount ({ user=u; pass=p }, c)));
  match sync (receive c) with Ok -> () | _ -> raise InvalidOperation

let add_viewer u p id viewer s =
  let c = new_channel () in
  sync (send s (AddViewer ({ user=u; pass=p }, id, viewer, c)));
  match sync (receive c) with Ok -> () | _ -> raise InvalidOperation


(*****************************************************************************)
(**************************** END OF HOMEWORK ********************************)
(*****************************************************************************)

(*****************************************************************************)
(* TESTS [do not change] *)
let reset () =
  Thread.tc := 0
let threads_created () =
  !Thread.tc

let d_server () =
  let s = document_server () in
  add_account "user1" "pass1" s;
  add_account "user2" "pass2" s;
  add_account "user3" "pass3" s;
  s

let tests = [
  (* 13.4 *)
  __LINE_OF__ (fun () -> let pinc = par_unary (fun x -> x + 1) in pinc [8;1;1] = [9;2;2] && threads_created () = 3);
  __LINE_OF__ (fun () -> let psof = par_unary string_of_float in psof [7.;1.] = ["7.";"1."] && threads_created () = 2);
  __LINE_OF__ (fun () -> let pmul = par_binary ( * ) in pmul [1;2;3] [5;6;2] = [5;12;6] && threads_created () = 3);
  __LINE_OF__ (fun () -> let pcon = par_binary ( ^ ) in pcon ["th";"";"ver";"nic"] ["is";"is";"y";"e"] = ["this";"is";"very";"nice"] && threads_created () = 4);
  (* 13.5
     NOTE: Array's functions cannot be tested in isolation, so if a test for size fails it may very well be due to a mistake in your implementation of make *)
  __LINE_OF__ (fun () -> let _ = Array.make 3 "abc" in threads_created () = 1);
  __LINE_OF__ (fun () -> let a = Array.make 3 1. in Array.destroy a; threads_created () = 1);
  __LINE_OF__ (fun () -> let a = Array.make 3 0 in Array.size a = 3);
  __LINE_OF__ (fun () -> let a = Array.make 3 'x' in Array.get 0 a = 'x');
  __LINE_OF__ (fun () -> let a = Array.make 3 'x' in try let _ = Array.get 3 a in false with OutOfBounds -> true);
  __LINE_OF__ (fun () -> let a = Array.make 3 0 in Array.set 1 5 a; Array.get 0 a = 0 && Array.get 1 a = 5 && Array.get 2 a = 0 && threads_created () = 1);
  __LINE_OF__ (fun () -> let a = Array.make 3 'x' in try Array.set 3 'u' a; false with OutOfBounds -> true);
  __LINE_OF__ (fun () -> let a = Array.make 3 0 in Array.resize 5 1 a; Array.size a = 5 && Array.get 2 a = 0 && Array.get 3 a = 1 && Array.get 4 a = 1 && threads_created () = 1);
  __LINE_OF__ (fun () -> let a = Array.make 3 0 in Array.resize 1 1 a; Array.size a = 1 && Array.get 0 a = 0 && threads_created () = 1);
  (* 13.6
     NOTE: Document server functions cannot be tested in isolation, so if a test for view fails it may very well be due to a mistake in your implementation of document_server *)
  __LINE_OF__ (fun () -> let _ = document_server () in threads_created () = 1); (* basic thread creation *)
  __LINE_OF__ (fun () -> let s = document_server () in add_account "user1" "pass1" s; true); (* add correct account *)
  __LINE_OF__ (fun () -> let s = d_server () in try add_account "user1" "***" s; false with InvalidOperation -> true); (* account exists already *)
  __LINE_OF__ (fun () -> let s = d_server () in publish "user2" "pass2" "My Document" s <> publish "user1" "pass1" "My Document" s); (* publish document *)
  __LINE_OF__ (fun () -> let s = d_server () in try let _ = publish "user1" "***" "My Document" s in false with InvalidOperation -> true); (* publish incorrect auth *)
  __LINE_OF__ (fun () -> let s = d_server () in try let _ = view "user1" "pass1" 0 s in false with InvalidOperation -> true); (* view invalid document *)
  __LINE_OF__ (fun () -> let s = d_server () in let d = publish "user1" "pass1" "text" s in "text" = view "user1" "pass1" d s); (* view correct *)
  __LINE_OF__ (fun () -> let s = d_server () in let d = publish "user1" "pass1" "text" s in try let _ = view "user2" "pass2" d s in false with InvalidOperation -> true); (* view, no access *)
  __LINE_OF__ (fun () -> let s = d_server () in try add_viewer "user1" "pass1" 0 "user3" s; false with InvalidOperation -> true); (* add viewer invalid document *)
  __LINE_OF__ (fun () -> let s = d_server () in let d = publish "user1" "pass1" "text" s in try add_viewer "user1" "***" d "user3" s; false with InvalidOperation -> (try let _ = view "user3" "pass3" d s in false with InvalidOperation -> true)); (* add viewer invalid auth *)
  __LINE_OF__ (fun () -> let s = d_server () in let d = publish "user2" "pass2" "text" s in add_viewer "user2" "pass2" d "user1" s; view "user1" "pass1" d s = "text"); (* add viewer correct *)
  __LINE_OF__ (fun () -> let s = d_server () in let d = publish "user1" "pass1" "mydoc" s in try change_owner "user1" "***" d "user2" s; false with InvalidOperation -> true); (* change owner invalid auth *)
  __LINE_OF__ (fun () -> let s = d_server () in try change_owner "user1" "pass1" 0 "user3" s; false with InvalidOperation -> true); (* change owner invalid document *)
  __LINE_OF__ (fun () -> let s = d_server () in let d = publish "user1" "pass1" "mydoc" s in try change_owner "user2" "pass2" d "user2" s; false with InvalidOperation -> true); (* change owner, not owner *)
  __LINE_OF__ (fun () -> let s = d_server () in let d = publish "user1" "pass1" "mydoc" s in change_owner "user1" "pass1" d "user3" s; view "user3" "pass3" d s = "mydoc"); (* change owner correct *)
]

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
    reset ();
    let ok = try t () with e -> print_endline (Printexc.to_string e); false in
    if not ok then fail l;
    ok
  in
  let passed = filter (fun x -> x) (map test tests) in
  printf "passed %d/%d tests\n" (length passed) (length tests)