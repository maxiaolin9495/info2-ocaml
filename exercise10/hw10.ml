
module type Ring = sig
  type t
  val zero : t
  val one : t
  val compare : t -> t -> int
  val to_string : t -> string
  val add : t -> t -> t
  val mul : t -> t -> t
end

module type Matrix = sig
  type elem
  type t
  val create : int -> int -> t
  val identity : int -> t
  val from_rows : elem list list -> t
  val to_string : t -> string
  val set : int -> int -> elem -> t -> t
  val get : int -> int -> t -> elem
  val transpose : t -> t
  val add : t -> t -> t
  val mul : t -> t -> t
end

(*****************************************************************************)
(**************************** HOMEWORK STARTS HERE ***************************)
(*****************************************************************************)
(* Assignment 10.2 [20 Points] *)

module IntRing = struct
  type t = int
  let zero = 0
  let one = 1
  let compare a b = if a > b then one else if a = b then zero else - one
  let to_string a = string_of_int a
  let add a b = a + b
  let mul a b = a * b
end

module FloatRing = struct
  type t = float
  let zero = 0.0
  let one = 1.0
  let compare a b = if a > b then 1 else if a = b then 0 else (-1)
  let to_string a = string_of_float a
  let add a b = a +. b
  let mul a b = a *. b
end

module type FiniteRing = sig
  include Ring
  val elems: t list
end

module BoolRing = struct
  type t = bool 
  let elems = [true;false]
  let zero = true
  let one = false
  let compare a b = if a = b then 0 else if a = true then -1 else 1
  let to_string a = string_of_bool a
  let add a b = a && b
  let mul a b = a || b
end

let rec throw_from_list l x output = match l with a::ls -> if a = x then throw_from_list ls x output else throw_from_list ls x (a::output)
| [] -> List.rev output

let rec reorder l1 output =let rec find_smallst l smallst =  match l with x::xs -> if x < smallst then find_smallst xs x else find_smallst xs smallst
  | [] -> smallst in 
  match l1 with a::ls ->  let smallst =find_smallst l1 a in reorder (throw_from_list l1 smallst []) (smallst::output)
  | [] -> List.rev output

module SetRing (R : FiniteRing) = struct
  type t = R.t list
  let zero = []
  let one = R.elems

  let elems = R.elems
  let add l1 l2 = 
    let rec add_help l1 l2 output = match l1, l2 with x::xs, y::ys -> if x = y then add_help xs ys (x::output) else if x < y then add_help xs l2 (x::output) else 
        add_help l1 ys (y::output)
        | [],[] -> List.rev output
        | [], _ -> (List.rev output) @ l2
        | _, [] -> (List.rev output) @ l1 in
      if l1 = [] then if l2 <> [] then reorder l2 [] else [] else reorder l1 [];
      add_help (reorder l1 [] ) (reorder l2 []) []

  let mul l1 l2 = let rec find a l = match l with x::xs -> if a = x then true else find a xs
    | [] -> false in
    let rec mul_help l1 l2 output = match l1 with [] -> List.rev output
    | x::xs -> if find x l2 then mul_help xs l2 (x::output) else mul_help xs l2 output in
  mul_help (reorder l1 []) (reorder l2 []) []

  let compare a b = let rec compare_help l1 l2 = match l1, l2 with [],[] -> 0
  | x::xs, []-> 1
  | [], _ -> -1
  | x::xs, y::ys -> if x < y then -1 else if x = y then compare_help xs ys else 1
  in 
  compare_help (reorder a []) (reorder b [])

  let to_string a = let rec to_string_help l output =match l with x::xs -> if output <> "" then to_string_help xs (output ^ "," ^ (R.to_string x)) else to_string_help xs (output ^ "{" ^ (R.to_string x)) 
    | [] -> output ^"}"
  in to_string_help (reorder a []) ""
  end


module DenseMatrix (R:Ring) = struct
type elem = R.t
type t = (R.t list) list
let create i j = 
  let rec create_line j output = if j > 0 then create_line (j-1) (R.zero::output)else output in
  let rec create_help i j output = if i > 0 then create_help (i-1) j ((create_line j [])::output) else output in
  create_help i j []

let from_rows l = l
let to_string mat = let rec to_string_help_help l output = match l with x::xs -> if output = "" then to_string_help_help xs (output ^ (string_of_int x)) 
    else to_string_help_help xs (output ^ " " ^ (string_of_int x)  ) 
  | [] -> output in 
  let rec to_string_help mat output = match mat with x::[] -> output ^ (to_string_help_help x "")
    | x::xs -> to_string_help xs (output ^ (to_string_help_help x "") ^ "\n")
    | [] -> output in
  to_string_help mat ""

  let set r c v m = let rec set_help_help c v output l= match l with x::xs -> if c = 0 then (List.rev output)@ (v::xs)
      else set_help_help (c-1) v (x::output) xs 
    | [] -> failwith "invalid matrix" in
    let rec set_help r c v m output = match m with x::xs -> if r = 0 then (List.rev output )@ ((set_help_help c v [] x)::xs)
        else set_help (r-1) c v xs (x::output)
      | [] -> failwith "invalid matrix" in
    set_help r c v m []

  let identity i = let mat = create i i in 
    let rec identity_help i output = if i = 0 then output else identity_help (i-1) (set (i-1) (i-1) R.one output) in
  identity_help i mat

  let get r c m = let rec get_help_help c l = match l with x::xs -> if c = 0 then x
      else get_help_help (c-1) xs 
    | [] -> failwith "invalid matrix" in
    let rec get_help r c m = match m with x::xs -> if r = 0 then get_help_help c x 
        else get_help (r-1) c xs
      | [] -> failwith "invalid matrix" in
    get_help r c m

  let transpose mat = let m = List.length mat in
    let n = match mat with x::xs -> List.length x
      | [] -> 0 in
    let rec transpose_help_help mat m n output = 
      if m = 0 then output else transpose_help_help mat (m-1) n ((get (m-1) (n-1) mat)::output) in
    let rec transpose_help mat m n output i = 
      if i = 0 then output else transpose_help mat m n ((transpose_help_help mat m i [])::output) (i-1) in
    transpose_help mat m n [] n

  let add a b = let rec add_help_help l1 l2 output = match l1,l2 with [],[] -> List.rev output
      | x::xs, y::ys -> add_help_help xs ys ((R.add x y)::output)
      | _,_ -> failwith "invalid matrix" in
    let rec add_help a b output = match a,b with [],[] -> List.rev output
      | x::xs, y::ys -> add_help xs ys ((add_help_help x y [])::output)
      | _,_ -> failwith "invalid matrix" in
    add_help a b []

  let mul a b = 
    let rec mul_help_help_help l1 l2 output = match l1,l2 with [],[] -> output 
      | x::xs,y::ys -> mul_help_help_help xs ys (R.add (R.mul x y) output) 
      | _, _ -> failwith "invalid matrix" in
    let rec mul_help_help l1 m2 output = match m2 with [] -> List.rev output
      | y::ys -> mul_help_help l1 ys ((mul_help_help_help l1 y R.zero)::output) in
    let rec mul_help m1 m2 output = match m1 with [] -> List.rev output
      | x::xs -> mul_help xs m2 ((mul_help_help x m2 [])::output)
      | _-> failwith "invalid matrix" in
    mul_help a (transpose b) []
end

module SparseMatrix (R:Ring) = struct
type elem = R.t
type t = (R.t list) list
let create i j = 
  let rec create_line j output = if j > 0 then create_line (j-1) (R.zero::output)else output in
  let rec create_help i j output = if i > 0 then create_help (i-1) j ((create_line j [])::output) else output in
  create_help i j []

let from_rows l = l
let to_string mat = let rec to_string_help_help l output = match l with x::xs -> if output = "" then to_string_help_help xs (output ^ (string_of_int x)) 
    else to_string_help_help xs (output ^ " " ^ (string_of_int x)  ) 
  | [] -> output in 
  let rec to_string_help mat output = match mat with x::[] -> output ^ (to_string_help_help x "")
    | x::xs -> to_string_help xs (output ^ (to_string_help_help x "") ^ "\n")
    | [] -> output in
  to_string_help mat ""

  let set r c v m = let rec set_help_help c v output l= match l with x::xs -> if c = 0 then (List.rev output)@ (v::xs)
      else set_help_help (c-1) v (x::output) xs 
    | [] -> failwith "invalid matrix" in
    let rec set_help r c v m output = match m with x::xs -> if r = 0 then (List.rev output )@ ((set_help_help c v [] x)::xs)
        else set_help (r-1) c v xs (x::output)
      | [] -> failwith "invalid matrix" in
    set_help r c v m []

  let identity i = let mat = create i i in 
    let rec identity_help i output = if i = 0 then output else identity_help (i-1) (set (i-1) (i-1) R.one output) in
  identity_help i mat

  let get r c m = let rec get_help_help c l = match l with x::xs -> if c = 0 then x
      else get_help_help (c-1) xs 
    | [] -> failwith "invalid matrix" in
    let rec get_help r c m = match m with x::xs -> if r = 0 then get_help_help c x 
        else get_help (r-1) c xs
      | [] -> failwith "invalid matrix" in
    get_help r c m

  let transpose mat = let m = List.length mat in
    let n = match mat with x::xs -> List.length x
      | [] -> 0 in
    let rec transpose_help_help mat m n output = 
      if m = 0 then output else transpose_help_help mat (m-1) n ((get (m-1) (n-1) mat)::output) in
    let rec transpose_help mat m n output i = 
      if i = 0 then output else transpose_help mat m n ((transpose_help_help mat m i [])::output) (i-1) in
    transpose_help mat m n [] n

  let add a b = let rec add_help_help l1 l2 output = match l1,l2 with [],[] -> List.rev output
      | x::xs, y::ys -> add_help_help xs ys ((R.add x y)::output)
      | _,_ -> failwith "invalid matrix" in
    let rec add_help a b output = match a,b with [],[] -> List.rev output
      | x::xs, y::ys -> add_help xs ys ((add_help_help x y [])::output)
      | _,_ -> failwith "invalid matrix" in
    add_help a b []

  let mul a b = 
    let rec mul_help_help_help l1 l2 output = match l1,l2 with [],[] -> output 
      | x::xs,y::ys -> mul_help_help_help xs ys (R.add (R.mul x y) output) 
      | _, _ -> failwith "invalid matrix" in
    let rec mul_help_help l1 m2 output = match m2 with [] -> List.rev output
      | y::ys -> mul_help_help l1 ys ((mul_help_help_help l1 y R.zero)::output) in
    let rec mul_help m1 m2 output = match m1 with [] -> List.rev output
      | x::xs -> mul_help xs m2 ((mul_help_help x m2 [])::output)
      | _-> failwith "invalid matrix" in
    mul_help a (transpose b) []
end


(*****************************************************************************)
(**************************** END OF HOMEWORK ********************************)
(*****************************************************************************)

(*****************************************************************************)
(* TESTS [do not change] *)
let (|=) a b =
  List.sort compare a = List.sort compare b

let check_string_representation s elems =
  if String.length s < 2 then false else
  if String.get s 0 <> '{' then false else
  if String.get s (String.length s - 1) <> '}' then false else
  String.sub s 1 (String.length s - 2)
  |> String.split_on_char ','
  |> List.map String.trim
  |> (|=) elems

let tests =
  (****************************
   * tests for 10.2 (IntRing) :
   * NOTE: Comment tests until you have completed your implementation of IntRing
   *)
  (*
  let implementsRingSignature (module M : Ring) = true in
  [
  __LINE_OF__ (fun () -> implementsRingSignature (module IntRing));
  __LINE_OF__ (fun () -> IntRing.compare 9 10 < 0 && IntRing.compare 10 9 > 0 && IntRing.compare 10 10 = 0);
  __LINE_OF__ (fun () -> IntRing.add 10 IntRing.zero = 10);
  __LINE_OF__ (fun () -> IntRing.mul 10 IntRing.one = 10);
  __LINE_OF__ (fun () -> IntRing.to_string 10 = "10");
  ] @ *)

  (******************************
   * tests for 10.2 (FloatRing) :
   * NOTE: Comment tests until you have completed your implementation of FloatRing
   *)
  (*
  let implementsRingSignature (module M : Ring) = true in
  [
  __LINE_OF__ (fun () -> implementsRingSignature (module FloatRing));
  __LINE_OF__ (fun () -> FloatRing.compare 9.5 10.0 < 0 && FloatRing.compare 10.0 9.5 > 0 && FloatRing.compare 10.0 10.0 = 0);
  __LINE_OF__ (fun () -> FloatRing.add 10.0 FloatRing.zero = 10.0);
  __LINE_OF__ (fun () -> FloatRing.mul 10.0 FloatRing.one = 10.0);
  __LINE_OF__ (fun () -> FloatRing.to_string 10.0 = "10.");
  ] @ *)

  (*****************************
   * tests for 10.2 (BoolRing) :
   * NOTE: Comment tests until you have completed your implementation of BoolRing
   *)
  (*
  let implementsFiniteRingSignature (module M : FiniteRing) = implementsRingSignature (module M) in
  [
  __LINE_OF__ (fun () -> implementsFiniteRingSignature (module BoolRing));
  __LINE_OF__ (fun () -> BoolRing.compare BoolRing.zero BoolRing.one < 0 && BoolRing.compare BoolRing.one BoolRing.zero > 0 && BoolRing.compare BoolRing.zero BoolRing.zero = 0);
  __LINE_OF__ (fun () -> BoolRing.add true BoolRing.zero = true && BoolRing.add false BoolRing.zero = false);
  __LINE_OF__ (fun () -> BoolRing.mul true BoolRing.one = true && BoolRing.mul false BoolRing.one = false);
  __LINE_OF__ (fun () -> BoolRing.to_string true = "true");
  __LINE_OF__ (fun () -> BoolRing.elems |= [true;false]);
  ] @ *)

  (****************************
   * tests for 10.2 (SetRing) :
   * NOTE: Comment tests until you have completed your implementation of SetRing
   *)
  (*
  let module TestRing : FiniteRing with type t = char = struct
    let cfrom x = (int_of_char x) - (int_of_char 'a')
    let cto x = char_of_int (x mod 4 + int_of_char 'a')

    type t = char
    let zero = 'a'
    let one = 'd'
    let compare = Pervasives.compare
    let to_string c = Printf.sprintf "'%c'" c
    let add a b = (cfrom a) + (cfrom b) |> cto
    let mul a b = (cfrom a) * (cfrom b) |> cto
    let elems = ['a'; 'b'; 'c'; 'd']
  end in
  let module SR = SetRing (TestRing) in
  [
  __LINE_OF__ (fun () -> SR.zero = [] && SR.one |= ['a'; 'b'; 'c'; 'd']);
  __LINE_OF__ (fun () -> SR.compare ['b';'d'] ['a'] > 0);
  __LINE_OF__ (fun () -> SR.compare ['c';'b'] ['c';'d'] < 0);
  __LINE_OF__ (fun () -> SR.compare ['a';'d'] ['d';'a'] = 0);
  __LINE_OF__ (fun () -> SR.add ['a';'b'] ['c';'b'] |= ['a';'b';'c']);
  __LINE_OF__ (fun () -> SR.add ['b';'d'] SR.zero |= ['b';'d']);
  __LINE_OF__ (fun () -> SR.mul ['a';'b'] ['c';'b'] |= ['b']);
  __LINE_OF__ (fun () -> SR.mul ['a';'b'] SR.one |= ['a';'b']);
  __LINE_OF__ (fun () -> check_string_representation (SR.to_string SR.one) ["'a'";"'b'";"'c'";"'d'"]);
  ] @ *)

  (********************************
   * tests for 10.2 (DenseMatrix) :
   * NOTE: Comment tests until you have completed your implementation of DenseMatrix
   * NOTE: from_rows and get have to be correct in order for these tests to work correctly!
   *)
  (*
  let module DM = DenseMatrix (IntRing) in
  let dm0 = DM.from_rows [[4;-2;1];[0;3;-1]] in
  let dm1 = DM.from_rows [[1;2];[-3;4];[3;-1]] in
  let check_dense m l =
    List.mapi (fun r row -> List.mapi (fun c col -> col = DM.get r c m) row) l |> List.flatten |> List.for_all (fun x -> x)
  in
  [
    __LINE_OF__ (fun () -> check_dense (DM.create 2 3) [[0;0;0];[0;0;0]]);
    __LINE_OF__ (fun () -> check_dense (DM.identity 3) [[1;0;0];[0;1;0];[0;0;1]]);
    __LINE_OF__ (fun () -> check_dense (DM.set 1 0 7 (DM.identity 2)) [[1;0];[7;1]]);
    __LINE_OF__ (fun () -> check_dense (DM.transpose dm0) [[4;0];[-2;3];[1;-1]]);
    __LINE_OF__ (fun () -> check_dense (DM.add dm0 dm0) [[8;-4;2];[0;6;-2]]);
    __LINE_OF__ (fun () -> check_dense (DM.mul dm0 dm1) [[13;-1];[-12;13]]);
    __LINE_OF__ (fun () -> (DM.to_string dm0) = "4 -2 1\n0 3 -1");
  ] @ *)

  (*********************************
   * tests for 10.2 (SparseMatrix) :
   * NOTE: Comment tests until you have completed your implementation of SparseMatrix
   * NOTE: from_rows and get have to be correct in order for these tests to work correctly!
   *)
  (*
  let module SM = SparseMatrix (IntRing) in
  let sm0 = SM.from_rows [[4;-2;1];[0;3;-1]] in
  let sm1 = SM.from_rows [[1;2];[-3;4];[3;-1]] in
  let check_sparse m l =
    List.mapi (fun r row -> List.mapi (fun c col -> col = SM.get r c m) row) l |> List.flatten |> List.for_all (fun x -> x)
  in
  [
    __LINE_OF__ (fun () -> check_sparse (SM.create 2 3) [[0;0;0];[0;0;0]]);
    __LINE_OF__ (fun () -> check_sparse (SM.identity 3) [[1;0;0];[0;1;0];[0;0;1]]);
    __LINE_OF__ (fun () -> check_sparse (SM.set 1 0 7 (SM.identity 2)) [[1;0];[7;1]]);
    __LINE_OF__ (fun () -> check_sparse (SM.transpose sm0) [[4;0];[-2;3];[1;-1]]);
    __LINE_OF__ (fun () -> check_sparse (SM.add sm0 sm0) [[8;-4;2];[0;6;-2]]);
    __LINE_OF__ (fun () -> check_sparse (SM.mul sm0 sm1) [[13;-1];[-12;13]]);
    __LINE_OF__ (fun () -> (SM.to_string sm0) = "4 -2 1\n0 3 -1");
  ] @ *)
  []


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


