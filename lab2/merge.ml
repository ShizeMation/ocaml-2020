(* Optional: Implement and test merge sort.  Make sure you understand
   the code that is given and fill in all the missing code marked by
   "TODO" *)

(* split xs into two even parts: ys and zs *)
let rec split (xs:'a list) (ys:'a list) (zs: 'a list) : 'a list * 'a list =
  match xs with
      [] -> (ys, zs)
    | hd :: tail -> split tail zs (hd :: ys)

(* Make sure you understand the above function.  In particular, make
   sure you understand how it splits xs into two equal parts. *)

(* merge lists xs and ys *)
(* two empty lists merge into an empty list *)
(* an empty list merges with a non-empty list yielding the latter, unchanged *)
(* two non-empty lists compare first elements, and prepend the smaller
 * of the two to the result of the recursive merge *)
let rec merge (xs:int list) (ys:int list) : int list =
  [] (* TODO: FIX ME *)

(* Sort original list os *)
(* an empty list is already sorted *)
(* a one-element list is already sorted *)
(* a multi-element list should be split
 * and recursively sorted, then merged *)
let rec mergesort (os:int list) : int list  =
  [] (* TODO: FIX ME *)

let int_data1 : int list = []
let int_data2 : int list = [5]
let int_data3 : int list = [3; 5]
let int_data4 : int list = [5; 3]
let int_data5 : int list = [5; 7; 4; 3; 2; 1; 6]

let test1 () = mergesort int_data1 = []
let test2 () = mergesort int_data2 = [5]
let test3 () = mergesort int_data3 = [3; 5]
let test4 () = mergesort int_data4 = [3; 5]
let test5 () = mergesort int_data5 = [1; 2; 3; 4; 5; 6; 7]

let tests = [test1; test2; test2; test3; test4; test5]

let rec runtests (tests : (unit -> bool) list) =
  match tests with
      [] -> ()
    | hd::tail -> 
      if hd () 
      then print_endline "success!" 
      else print_endline "failure :-(";
      runtests tail

let main = 
  print_string "\n\nBeginning Tests\n\n"; 
  runtests tests;
  print_string "\nCompleted Tests\n\n"; 

