(*** CSI 3120 Assignment 2 ***)
(*** YOUR NAME HERE ***)
(*** YOUR STUDENT ID HERE ***)
(*** OCAML 4.05.0 ***)
(* If you use the version available from the lab machines via VCL, the
   version is 4.05.0 ***)

(*************)
(* PROBLEM 1 *)
(*************)

(* For each part of problem 1, explain in the given string why the code
   will not typecheck, then follow the instructions for that part to
   change the code so that it typechecks while keeping the same values
   as intended by the erroneous code. Once you have done so, uncomment
   the fixed expression for submission.
*)

(* Problem 1a - Give your explanation in exp1a and then fix the
   right-hand-side of the assignment to match the listed type.
   (Do not change the left-hand-side.)
*)

let exp1a : string = "It expects a list of tuples, but it got a list containing multiple types instead, which is illegal."
let prob1a : (string * int * char) list = [("7", 8, '9')];;

(* Problem 1b - Give your explanation in exp1b and then fix the type
   of variable prob1b to match the type of the expression on the
   right-hand-side of the assignment. (Do not change the
   right-hand-side.)
 *)

let exp1b : string = "It expected a list of tuples, but it got a tuple of lists."
let prob1b : (string list * int list) = (["apples";"bananas";"carrots"],[3;2;1]);;

(* Problem 1c - Give your explanation in exp1c and then fix the
   right-hand-side of the expression to match the variable prob1c's
   listed type.  (Do not change the left-hand-side.)
 *)

let exp1c : string = "It expects a list of lists, but it got a list of nested lists of various nesting depths."
let prob1c : string list list = [["2"; "b"]; ["or"; "not"; "2b"]; ["that is"; "the"]; ["question"]];;

(*************)
(* PROBLEM 2 *)
(*************)

(* Fill in expressions to satisfy the following types:
 *
 * NOTE: for option, list, and function types, you must
 * provide a nontrivial answer. For a list that means a
 * non-empty one, for an option type that means a Some
 * construction, and for a function, that means using
 * its arguments to generate the return value.
 * example problems:
 *   let x : int option = ???
 *   let y : int list = ???
 *   let f (x: int) (y: int) : int = ???
 * incorrect answers:
 *   let x : int option = None
 *   let y : int list = []
 *   let f (x: int) (y: int) : int = 7
 * possible correct answers:
 *   let x : int option = Some 1
 *   let y : int list = [1]
 *   let y : int list = [1; 2]
 *   let y : int list = 1 :: [2]
 *   let f (x: int) (y: int) : int = x + y
 *   let f (x: int) (y: int) : int =
 *         String.length  ((string_of_int x) ^ (string_of_int y))
 *)

(* Problem 2a *)

let prob2a : (int * ((string * float) option list)) list =
   [(69, ([Some ("guu", 420.0); Some ("wow", 7.0)]))];;


(* Problem 2b *)
(* a pet is a (name, animal_type, age option) tuple *)

type pet = string * string * int option

let prob2b : string * pet list option =
   ("Pets", Some [("Jake", "Dog", Some 34); ("Finn", "Human", Some 17)]);;

(* Problem 2c *)
(* Fill in a valid function call to f to make prob2c typecheck *)

let prob2c =
  let rec f arg =
    match arg with
    | (a, b) :: xs -> if a then (b ^ (f xs)) else f xs
    | _ -> ""
  in f [
      (true, "s");
      (false, "h");
      (false, "e");
      (true, "b");
      (true, "e");
      (false, "l");
      (false, "i");
      (false, "e");
      (true, "v");
      (true, "e");
      (false, "d")
   ];;

(*************)
(* PROBLEM 3 *)
(*************)

(* Problem 3a.  You have been asked to write a text filter,
   where you want to find all search characters in your text
   if they appear the right order.

   Write a function text_filter that takes two lists of characters
   and checks to see if all the characters in the first list are
   included in the second list AND in the same order, BUT possibly
   with other characters in between.  For example

   text_filter ['a';'m';'z'] ['1';'a';'2';'m';'3';'z'] = true
   text_filter ['a';'m';'z'] ['1';'a';'3';'z'] = false
   text_filter ['a';'m';'z'] ['1';'z';'2';'m';'3';'a'] = false
   *)

let rec text_filter (xs:char list) (ys:char list) : bool =
   match (xs, ys) with
   | ([], _) -> true
   | (_, []) -> false
   | (xh::xt, yh::yt) ->
      if xh = yh then text_filter xt yt
      else text_filter xs yt;;

(* Problem 3b. Rewrite the function above so that is is polymorphic,
   i.e., it should work on lists whose elements are any types.  Give
   at least one test case (call your function at least once) with a
   type that is different from chars. *)

let rec text_filter_poly (xs) (ys) : bool =
   match (xs, ys) with
   | ([], _) -> true
   | (_, []) -> false
   | (xh::xt, yh::yt) ->
      if xh = yh then text_filter_poly xt yt
      else text_filter_poly xs yt;;

let test3b_1 = text_filter_poly [4; 6; 9] [87; 5; 4; 6; 0; 12; 9] (* = true *)
let test3b_2 = text_filter_poly [4.3; 6.1; 9.6] [87.0; 4.3; 12.8; 9.6] (* = false *)
let test3b_3 = text_filter_poly ["mlem"; "poot"] ["hh"; "mlem"; "kap"; "poot"] (* = true *)

(*************)
(* PROBLEM 4 *)
(*************)

(* Write a function (int_to_whole) that converts an integer
   into a whole number if one exists
   (a whole number is 1, 2, 3, ...).
   Use an option type because not all integer inputs can
   be converted. *)

type whole = One | Succ of whole

let int_to_whole (x: int) : whole option =
   if x < 1 then None
   else
      let rec convert n =
         if n = 1 then One
         else Succ (convert (n-1))
      in Some (convert x);;
