let todo _ = failwith "todo"

(* type definitions *)
type 'a tree = Empty | Node of 'a * 'a tree * 'a tree
type 'a ltree = LNode of 'a * (unit -> 'a ltree) * (unit -> 'a ltree)

(*****************************************************************************)
(**************************** HOMEWORK STARTS HERE ***************************)
(*****************************************************************************)
(* Assignment 8.5 [3 Points] *)
let interleave3 l1 l2 l3 = let rec help l1 l2 l3 output = 
  match l1, l2, l3 with 
  [], [], [] -> List.rev output
  | x::xs, _, _-> help l2 l3 xs (x::output)
  | [], _, _ -> help l2 l3 [] output
in help l1 l2 l3 []


(*****************************************************************************)
(* Assignment 8.6 [4 Points] *)
let rec lagrange input = let rec lagrange_help input output x_list = match input with
    [] -> output
  | (a,b)::ls -> lagrange_help ls (fun x -> b *. (iterator a x_list (fun c -> 1.) x) +. output x) x_list
  in lagrange_help input (fun x -> 0.) (get_x_list input [])
and get_x_list l x_list = match l with [] -> List.rev x_list
| (a,b)::xs -> get_x_list xs (a::x_list)
and iterator x x_list output = match x_list with [] -> output
| x'::xs -> if x = x' then iterator x xs (fun a -> output a)
            else iterator x xs (fun a -> (a -. x')/. (x -. x') *. (output a))


(*****************************************************************************)
(* Assignment 8.7 [6 Points] *)
let rec insert value comparefun tree = match tree with Empty -> Node(value, Empty, Empty)
| Node(x, a, b) -> if comparefun value x < 0 then Node(x, insert value comparefun a, b) 
                   else if comparefun value x > 0 then Node(x, a, insert value comparefun b)
                   else tree

let rec string_of_tree outputFun tree = match tree with Empty -> "Empty" 
| Node(x,a,b) -> "Node(" ^ (outputFun x) ^ ", " ^ string_of_tree outputFun a ^ ", " ^ string_of_tree outputFun b ^ ")"
let rec inorder_list t = 
          match t with Empty -> []
                     | Node(v, l, r) -> List.rev_append (List.rev (inorder_list l)) (v::inorder_list r);;


(*****************************************************************************)
(* Assignment 8.8 [7 Points] *)
let rec layer_tree r = LNode(r, (fun () -> layer_tree (r + 1)), (fun () -> layer_tree (r + 1)))

let rec interval_tree (a,b) = LNode ((a,b), (fun () -> interval_tree (a, (a+.b)/. 2.)), fun () -> interval_tree (((a+.b)/. 2.), b))

let rational_tree x = let rec rational_tree_help (a,b) = LNode((a,b), (fun () -> rational_tree_help (a, b+1)), fun () -> rational_tree_help (a+1, b))
in rational_tree_help (0,0)

let rec top n ltr = if n = 0 
then Empty else match ltr with LNode(a,b,c) -> Node(a, top (n-1) (b ()), top (n-1) (c ()))

let rec map funA (LNode(a,b,c)) = LNode(funA a, (fun () -> (map funA (b ()))), fun () -> (map funA (c ())))

let rec find condition (LNode(a,b,c)) = if condition a then LNode(a, b, c)
                                        else match b (), c() with LNode(d,e,f), LNode(g,h,i) ->
                                          if compare d a = 0 && compare g a = 0 then failwith "unreachable" 
                                          else if compare d a = 0 then find condition (c())
                                            else if compare g a = 0 then find condition (b()) 
                                              else find condition (b())                                              
and combine_ltrees (LNode(a,b,c)) (LNode(d,e,f)) = if compare a d < 0 then LNode(d, (fun() -> LNode(a,b,c)),fun() -> LNode(d,e,f))
else if compare a d = 0 then LNode(a,b,c)
else LNode(a, (fun() -> LNode(d,e,f)),fun() -> LNode(a,b,c))

(*****************************************************************************)
(**************************** END OF HOMEWORK ********************************)
(*****************************************************************************)
(* example inputs, you may use them to test your implementations,
   but [do not change] *)
type 'a a85_test_input = { l1 : 'a list; l2 : 'a list; l3 : 'a list }
let a85_ex1 = { l1 = [0;1;2]; l2 = [10;11;12]; l3 = [20;21;22] }
let a85_ex2 = { l1 = ['a';'b']; l2 = ['A';'B';'C';'D']; l3 = ['!'] }
let a85_ex3 = { l1 = []; l2 = []; l3 = [] }

type a86_test_input = { points : (float * float) list; poly : float -> float }
let a86_ex1 = { points=[100.,231.]; poly=fun x -> 231. }
let a86_ex2 = { points=[100.,231.; 200.,12.]; poly=fun x -> 450. -. 2.19 *. x }
let a86_ex3 = { points=[100.,231.; 200.,12.; 300.,382.5]; poly=fun x -> 0.029475 *. x *. x -. 11.0325 *. x +. 1039.5 }

let a88_ex1 = let rec b () = LNode ('a', b, b) in b ()
let a88_ex2 = let rec b s () = LNode (s, b (0::s), b (1::s)) in b [] ()
let a88_ex3 = let rec b i () = LNode (i * i, b (i+1), b (i+1)) in b 0 ()
let a88_ex4 = let rec b x () = LNode (x, b (x * 10), b (x + 10)) in b 0 ()
let a88_ex5 = let rec b x () = LNode (x, b (x * 10), b (x + 10)) in b 1000 ()


(*****************************************************************************)
(* TESTS [do not change] *)
let (=.) a b = (abs_float (a -. b)) < 0.001
let (=~) a b =
  let trimmed s = String.split_on_char ' ' s |> List.filter (fun x -> (String.length x) > 0) |> String.concat "" in
  String.equal (trimmed a) (trimmed b)
let is_interleave3_tailrec () =
  let l = List.init 1000000 (fun _ -> 1) in
  try ignore(interleave3 l l l); ignore(interleave3 [] l []); true with Stack_overflow -> false
let compare_polys (p : float -> float) (ex : a86_test_input) : bool =
  (List.for_all (fun (x,y) -> (p x) =. y) ex.points) &&
  (List.init 10 (fun x -> 37.5 *. float_of_int x) |> List.for_all (fun x -> (p x) =. (ex.poly x)))
let insert_ vs cmp t =
  List.fold_left (fun t v -> insert v cmp t) t vs
let is_inorder_list_tailrec () =
  ignore(inorder_list Empty);
  (* TODO: Tutors will check *)
  (* let l = List.init 10000 (fun x -> x) in
  let t = insert_ l compare Empty in
  try ignore(inorder_list t); true with Stack_overflow -> false *)
  true
let check_layer_tree r t =
  let rec impl n r (LNode (x, fl, fr)) =
    if n <= 0 then true else r = x && (impl (n-1) (r+1) (fl ())) && (impl (n-1) (r+1) (fr ()))
  in
  impl 4 r t
let check_interval_tree i t =
  let rec impl n (l,h) (LNode ((l',h'), fl, fr)) =
    if n <= 0 then true else (l =. l') && (h =. h') && (impl (n-1) (l, (l+.h)/.2.) (fl ())) && (impl (n-1) ((l+.h)/.2., h) (fr ()))
  in
  impl 4 i t
let check_rational_tree t =
  let rec impl n (a,b) (LNode ((a',b'), fl, fr)) =
    if n <= 0 then true else (a = a') && (b = b') && (impl (n-1) (a, b+1) (fl ())) && (impl (n-1) (a+1, b) (fr ()))
  in
  impl 4 (0,0) t
let rec compare_ltrees n (LNode (x1, lf1, rf1)) (LNode (x2, lf2, rf2)) =
  if n <= 0 then true else x1 = x2 && (compare_ltrees (n-1) (lf1 ()) (lf2 ())) && (compare_ltrees (n-1) (rf1 ()) (rf2 ()))

let tests = [
  (* tests for 8.5 *)
  __LINE_OF__ (fun () -> (interleave3 a85_ex1.l1 a85_ex1.l2 a85_ex1.l3) = [0;10;20;1;11;21;2;12;22]);
  __LINE_OF__ (fun () -> (interleave3 a85_ex2.l1 a85_ex2.l2 a85_ex2.l3) = ['a';'A';'!';'b';'B';'C';'D']);
  __LINE_OF__ (fun () -> (interleave3 a85_ex3.l1 a85_ex3.l2 a85_ex3.l3) = []);
  __LINE_OF__ (fun () -> is_interleave3_tailrec ());
  (* tests for 8.6 *)
  __LINE_OF__ (fun () -> (let l = lagrange a86_ex1.points in compare_polys l a86_ex1));
  __LINE_OF__ (fun () -> (let l = lagrange a86_ex2.points in compare_polys l a86_ex2));
  __LINE_OF__ (fun () -> (let l = lagrange a86_ex3.points in compare_polys l a86_ex3));
  (* tests for 8.7 *)
  __LINE_OF__ (fun () -> (insert 3 compare Empty) = Node (3, Empty, Empty));
  __LINE_OF__ (fun () -> (insert 3 compare (Node (6, Empty, Empty))) = Node (6, Node (3, Empty, Empty), Empty));
  __LINE_OF__ (fun () -> (insert_ [1;3;8;2;7;4;9] compare Empty) = Node (1, Empty, Node (3, Node (2, Empty, Empty), Node (8, Node (7, Node (4, Empty, Empty), Empty), Node (9, Empty, Empty)))));
  __LINE_OF__ (fun () -> (insert 3 (fun a b -> b - a) (Node (6, Empty, Empty))) = Node (6, Empty, Node (3, Empty, Empty)));
  __LINE_OF__ (fun () -> (insert_ [(3,2);(10,-2);(18,20)] (fun (a1,a2) (b1,b2) -> compare (a1 + a2) (b1 + b2)) Empty) = Node ((3,2), Empty, Node ((10,-2), Empty, Node ((18,20), Empty, Empty))));
  __LINE_OF__ (fun () -> (string_of_tree (fun _ -> "") Empty) = "Empty");
  __LINE_OF__ (fun () -> (string_of_tree (fun _ -> "xx") (Node (2.5, Empty, Empty))) =~ "Node (xx, Empty, Empty)");
  __LINE_OF__ (fun () -> (string_of_tree string_of_int (Node (3, Empty, Node (1, Empty, Empty)))) =~ "Node (3, Empty, Node (1, Empty, Empty))");
  __LINE_OF__ (fun () -> (inorder_list Empty) = []);
  __LINE_OF__ (fun () -> (inorder_list (Node (2, Node (1, Empty, Empty), Node (3, Empty, Empty)))) = [1;2;3]);
  __LINE_OF__ (fun () -> (inorder_list (Node (1, Empty, Node (3, Node (2, Empty, Empty), Node (8, Node (7, Node (4, Empty, Empty), Empty), Node (9, Empty, Empty)))))) = [1;2;3;4;7;8;9]);
  __LINE_OF__ (fun () -> (is_inorder_list_tailrec ()));
  (* tests for 8.8 *)
  __LINE_OF__ (fun () -> check_layer_tree 0 (layer_tree 0));
  __LINE_OF__ (fun () -> check_interval_tree (0., 10.) (interval_tree (0., 10.)));
  __LINE_OF__ (fun () -> check_rational_tree (rational_tree ()));
  __LINE_OF__ (fun () -> (top 0 a88_ex1) = Empty);
  __LINE_OF__ (fun () -> (top 1 a88_ex1) = Node ('a', Empty, Empty));
  __LINE_OF__ (fun () -> (top 3 a88_ex1) = (Node ('a', Node ('a', Node ('a', Empty, Empty), Node ('a', Empty, Empty)), Node ('a', Node ('a', Empty, Empty), Node ('a', Empty, Empty)))));
  __LINE_OF__ (fun () -> (top 3 a88_ex2) = (Node ([], Node ([0], Node ([0;0], Empty, Empty), Node ([1;0], Empty, Empty)), Node ([1], Node ([0;1], Empty, Empty), Node ([1;1], Empty, Empty)))));
  __LINE_OF__ (fun () -> (compare_ltrees 3 a88_ex1 (map (fun _ -> 'a') a88_ex2)));
  __LINE_OF__ (fun () -> (compare_ltrees 3 a88_ex3 (map (fun x -> let l = List.length x in l * l) a88_ex2)));
  __LINE_OF__ (fun () -> (compare_ltrees 3 a88_ex1 (find (fun x -> x = 'a') a88_ex1)));
  __LINE_OF__ (fun () -> (compare_ltrees 3 a88_ex5 (find (fun x -> x >= 1000) a88_ex4)));
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
    let ok = try t () with e -> print_endline (Printexc.to_string e); false in
    if not ok then fail l;
    ok
  in
  let passed = filter (fun x -> x) (map test tests) in
  printf "passed %d/%d tests\n" (length passed) (length tests)
