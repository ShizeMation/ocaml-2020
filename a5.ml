(*** CSI 3120 Assignment 5 ***)
(*** Shize Li ***)
(*** YOUR STUDENT ID HERE ***)
(*** OCAML 4.05.0 ***)
(* If you use the version available from the lab machines via VCL, the
   version is 4.05.0 ***)

(****************************************************************************)
(* PROBLEM 1: Activation stacks (heaps) for functions returned as arguments *)
(****************************************************************************)

(* Consider the OCaml code below. *)

let _ =
  let z = 3 in
  let b = (z = 3) in
  let g w =
    (let x = (b || w = 4) in
     let h y = if y then (b && x) else (z < w) in
     h) in
  let f = g 1 in
  let z = 0 in
  f (z <> 3)

(* Show the activation heap for the execution of this code.  (It is
   called a heap here because it involves a function call that returns
   a function, whose activation record cannot be popped.)  For this
   question, assume that no activation records will be popped and that
   they will be garbage collected later.  In your activation records,
   include control links, access links, local variables, parameters,
   and return-result address for calls to f.  Also, in activation
   records for calls to g, include an intermediate result for (w=4)
   and in activation records for calls to f, include intermediate
   results for the values of (b && x) and (z < w). *)


(*****************************)
(* PROBLEM 2: Tail recursion *)
(*****************************)

(* Below is a tail recursive version of the factorial function similar
   to the one in the textbook and course notes. *)

let tlfact n =
  let rec aux n a =
    if n<=1 then a else aux (n-1) (n*a) in
  aux n 1

(* Write a tail recursive version of a function that takes a list of
   floats and returns the minimum as a float option.  If the list is
   empty, return None.  *) 

let tlmin (xs:float list) : float option =
   if xs = [] then None
   else
   let (xs_head::xs_tail) = xs in
   if xs_tail = [] then Some (xs_head)
   else
   let rec aux (l:float list) (min:float) : float =
      match l with
      | [x] -> if x < min then x else min
      | head::tail ->
         if head < min then
            aux tail head
         else
            aux tail min
   in
   Some (aux xs_tail xs_head)

(******************************************)
(* PROBLEM 3: Programming with exceptions *)
(******************************************)

(* Problem 3(a) *)
(* Write a function that finds the product of a list of positive
   floats (i.e., multiplies them all together).  The product of an
   empty list is 1.0.  This function should raise an exception in two
   distinct cases.  First, raise an exception that takes no arguments
   for the case when one of the numbers is equal to 0.0.  This one
   will be used for efficiency.  Second, raise an exception that takes
   one argument in the case when the list contains at least one number
   that is less than 0.0. The data returned when this exception is
   raised should be the minimum number in the input list.  You may
   call your function tlmin to calculate the minimum. Some test code
   is shown below providing examples that illustrate the behavior of
   this function. *)

exception ProductIsZero
exception NegativeInList of float
let rec product (xs:float list) : float =
   match xs with
   | [] -> 1.0
   | head::tail ->
      if head = 0.0 then raise ProductIsZero
      else if head < 0.0 then 
         let Some n = tlmin xs in
         raise (NegativeInList n)
      else head *. product tail

(* let test3a1 = product [3.2;0.4;90.3;1.8;4.6;90.0]
   val test3a1 : float = 86133.1968
   let test3a2 = product [3.2;0.4;90.3;-1.8;4.6;-90.0]
   Exception: NegativeInList (-90.).
   let test3a3 = product [3.2;0.4;0.0;-1.8;4.6;-90.0]
   Exception: ProductIsZero. *)


(* Problem 3(b) *)
(* Write a function that calls your function from question 3(a)
   and returns a string. (Replace the empty string in the definition
   below with a correct funcion body.  It must handle all exceptions.
   The examples below illustrate what string should be returned in
   each case (including all cases where an exception is raised or
   not). *)

let try_product (xs:float list) : string =
   try string_of_float (product xs) with
   | ProductIsZero -> "0."
   | NegativeInList n -> "List contains a negative number; minimum is " ^ string_of_float n

let test3b1 = try_product [3.2;0.4;90.3;1.8;4.6;90.0]
(* val test3b1 : string = "86133.1968" *)
let test3b2 = try_product [3.2;0.4;90.3;-1.8;4.6;-90.0]
(* val test3b2 : string = "List contains a negative number; minimum is -90." *)
let test3b3 = try_product [3.2;0.4;0.0;-1.8;4.6;-90.0]
(* val test3b3 : string = "0." *)


(*********************************************)
(* PROBLEM 4: Call by need parameter passing *)
(*********************************************)

(* The code below is from the textbook and course notes. *)

type 'a delay =
  | EV of 'a
  | UN of (unit -> 'a)

let ev (d:'a delay) =
  match d with
  | EV x -> x
  | UN f -> f()

let force (d:'a delay ref) =
  let v = ev !d in
  (d := EV v; v)

let rec fib (n:int) =
  if n=0 || n=1 then 1 else fib (n-1) + fib (n-2)

(* Consider the code below defining and using the function f4. *)

let f4 (x:int) (y:int) (z:int) : int =
  if z < 0 then x else y

let m1 = f4 (fib 14) (fib 41) 1
let n1 = f4 (fib 14) (fib 41) (-1)

(* Problem 4(a) *)
(* The values m1 and n1 are calculated by two different calls to f4.
   Does one call execute faster than the other?  If so, which one is
   faster and why?  If not, explain why they take (roughly) the same
   amount of time.
   
   ANSWER:

   They take the same amount of time to execute because
   the parameters are all evaluated before starting f4.
   It doesn't matter which one gets returned.

   *)
  

(* Problem 4(b) *)
(* Write a call-by-need version of the f4 function from 4(a).  The
   first two arguments should be of type "int delay ref" instead of
   type "int".  Do not change the type of the third argument.  The
   first two arguments should only be evaluated if needed. *)

let f4_cbn (x:int delay ref) (y:int delay ref) (z:int) : int =
   if z < 0 then force x else force y

(* Problem 4(c) *)
(* Calculate m2 and n2 below by calling your function f4_cbn from 4(b)
   twice, with x=(fib 14) and y=(fib 41) in both calls, and with z=1
   in the first call and z=(-1) in the second call, similar to the
   calls above for calculating m1 and n1 using f4.  Does one call
   execute faster than the other?  If so, which one is faster and why?
   If not, explain why they take (roughly) the same amount of time.
   Then calculate m2' by repeating the same call to f4_cbn as you did
   for calculating m2.  Why is the calculation of m2' faster? *)

let fib14 = ref (UN (fun () -> fib 14))
let fib41 = ref (UN (fun () -> fib 41))

let m2 = f4_cbn fib14 fib41 1
let n2 = f4_cbn fib14 fib41 (-1)
let m2' = f4_cbn fib14 fib41 1
