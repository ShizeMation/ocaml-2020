(* 

  Agenda:
   * tuples and lists
   * options
   * higher order functions

  Note that questions 2, 8, and 9 are optional.

*)

(* An employee is a tuple of name, age, and boolean indicating marriage status *)
type employee = string * int * bool
                                 
(* 1. Write a function that takes an employee, and prints out the information in some readable form. *)

let print_employee_info (t:employee) = 
   let (name, age, married) = t in
      print_endline ("Name: " ^ name);
      print_endline ("Age: " ^ string_of_int age);
      print_endline ("Is married?: " ^ string_of_bool married)

(* 2. Reimplement the OCaml standard functions List.length and List.rev
   for lists of strings.
   This question is optional, but is good practice for the next one. *)

(*
let length (l:string list) : int = 

let rev (l:string list) : string list = 
*)

(* 3. Remove the kth element from a list. Assume indexing from 0 *)
(* example: rmk 2 ["to" ; "be" ; "or" ; "not" ; "to" ; "be"] 
 * results in: [["to" ; "be" ; "not" ; "to" ; "be"] *)

let rec rmk (k:int) (l:string list) : string list =
   if k = 0 then
      match l with
      | [] -> []
      | _::tail -> tail
   else
      match l with
      | [] -> failwith "invalid input"
      | head::tail -> head::(rmk (k - 1) tail)

(* 4. Write a function that returns the final element of an integer list,
   if it exists, and None otherwise *)

let rec final (l: int list) : int option =
   match l with
   | [] -> None
   | n::[] -> Some n
   | _::tail -> final tail

(* 5. Write a function to return the smaller of two int options, or None
 * if both are None. If exactly one argument is None, return the other. Do 
 * the same for the larger of two int options.*)

let min_option (x: int option) (y: int option) : int option =
   match (x, y) with
   | (None, None) -> None
   | (None, Some k) -> Some k
   | (Some n, None) -> Some n
   | (Some n, Some k) -> if k < n then Some k else Some n

let max_option (x: int option) (y: int option) : int option =
   match (x, y) with
   | (None, None) -> None
   | (None, Some k) -> Some k
   | (Some n, None) -> Some n
   | (Some n, Some k) -> if k > n then Some k else Some n

(* 6. Write a function that returns the integer buried in the argument
 * or None otherwise *)  

let get_option (x: int option option option option) : int option =
   match x with
   | None -> None
   | Some None -> None
   | Some (Some None) -> None
   | Some (Some (Some None)) -> None
   | Some (Some (Some (Some n))) -> Some n

(* 7. Write a function to return the boolean AND/OR of two bool options,
 * or None if both are None. If exactly one is None, return the other. *)

let and_option (x:bool option) (y: bool option) : bool option =
   match (x, y) with
   | (None, None) -> None
   | (None, Some k) -> Some k
   | (Some n, None) -> Some n
   | (Some n, Some k) -> Some (n && k)

let or_option (x:bool option) (y: bool option) : bool option =
   match (x, y) with
   | (None, None) -> None
   | (None, Some k) -> Some k
   | (Some n, None) -> Some n
   | (Some n, Some k) -> Some (n || k)

(* What's the pattern? How can we factor out similar code? *)

(**************)
(* Note: Questions 8 and 9 are optional.  We have not yet covered all
   of this material in class, but you still may be able to do them,
   especially if you have read ahead in the class notes. *)
(**************)
                                 
(* 8. Optional:
 * Write a higher-order function for binary operations on options.
 * If both arguments are present, then apply the operation.
 * If both arguments are None, return None.  If one argument is (Some x)
 * and the other argument is None, function should return (Some x) *)
(* What is the type of the calc_option function? *)

(*
let calc_option (f: 'a->'a->'a) (x: 'a option) (y: 'a option) : 'a option =  
*)

(* 9. Optional:
 * Now rewrite the following functions using the above higher-order function
 * Write a function to return the smaller of two int options, or None
 * if both are None. If exactly one argument is None, return the other.
 * Do the same for the larger of two int options. *)

(*
let min_option2 (x: int option) (y: int option) : int option = 
*)

(*
let max_option2 (x: int option) (y: int option) : int option = 
*)
