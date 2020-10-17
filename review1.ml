(* For each question in the Brightspace quiz, copy the code and
   question here, and follow the instructions to answer it.  Do this
   for all questions except those where you have to draw trees.  (You
   can do those on a separate piece of paper. *)

(* Question 1: Inductive Data Types

   (Question 1a can be answered on a separate piece of paper.) *)

   type tree = 
      | Empty
      | Node of tree * int * tree
      
   (* helper function for constructing trees *)
   let leaf (i:int) : tree = Node(Empty, i, Empty)

   let t1 = 
   Node(Node(leaf 2, 1, Empty),
               0,
               Node(leaf 7, 6, leaf 8))

   (* Question 1a. Draw the tree represented by t1. Answer this one on a separate sheet of paper.*)

   (* Question 1b. What does the following function do? Replace the empty
      string below with your answer. *)

   let fun_1b (t:tree) : bool =
   begin match t with
   | Node(Empty,_,Empty) -> true
   | _ -> false
   end

   let answer1b : string = "Determines if the tree is just one leaf."

   (* Question 1c. What is the value of l1 below? Add a comment with
      your answer. What does the function fun_1c do? Replace the empty
      string below with your answer. *)

   let rec fun_1c (t:tree) : int list =
   begin match t with
   | Empty -> []
   | Node(l,n,r) -> n::(fun_1c l)@(fun_1c r)
   end

   let l1 = fun_1c t1

   let answer1c : string = "[0;1;2;6;7;8]"

(* Question 2. Polymorphic and Higher-Order Programming *)

   let add (x:int) (y:int) : int = x + y
   let mult (x:int) (y:int) : int = x * y

   let rec fun2 x n =
   match n with
   | 0 -> []
   | n -> if n < 0 then []
            else x::(fun2 x (n-1))

   (* Question 2a. What does the fun2 function do? Replace the empty
      string below with your answer. *)

   let answer2a : string = "Creates a list with n copies of x."

   (* Question 2b. What is the type of fun2? Add a comment with your answer *)

   (* 'a -> int -> 'a list *)

   (* Question 2c. What are the types and values of a2, b2, c2, and d2
      below? Add a comment with your answers. *)

   let a2 = fun2 true 4
   let b2 = fun2 add 2
   let c2 = fun2 (add 3) 2
   let d2 = fun2 (fun2 false 3) 2

   (* 
      a2: bool list = [true; true; true; true]
      b2: (int -> int -> int) list = [add; add]
      c2: (int -> int) list = [(add 3); (add 3)]
      d2: (bool list) list = [[false; false; false]; [false; false; false]]
   *)

   (* Question 2d. What are the types of e2, f2, g2, h2, i2, j2 below.
      Add a comment with your answers? *)

   let e2 = (1,false)
   let f2 = [(1,false);(2,true)]
   let g2 = ([1;2],[false;true])
   let h2 = fun2 4.3
   let i2 = fun x -> x < 5
   let j2 = [add;mult]

   (* 
      e2: int * bool
      f2: (int * bool) list
      g2: (int list) * (bool list)
      h2: int -> float list
      i2: int -> bool
      j2: (int -> int -> int) list
   *)

(* Question 3. Parsing and Precedence
   (All parts of Question 3 can be answered on a separate piece
   of paper.) *)

   (* 3a. 
         1 - 1 * 1
         1 - 1
         0
      3b.

   *)
   

(* Question 4: Anonymous Functions *)

   (* What is the value of x4? Add a comment with your answer. To test
      your understanding, find the answer without executing the code. *)
         
   let rec guess (f:int -> int -> int) (l:int list) : int option =
   match l with
   | [] -> None
   | [a] -> Some a
   | a::b::l' -> guess f ((f a b)::l')

   let x4 = guess (fun x -> fun y -> (x * x) + y) [2;3;5]

   (* ...[2,3,5]
      ...[7,5]
      ...[54]
      => Some 54
   *)

(* Question 5: More Inductive Data Types *)

   type ('a,'b) newtype =
      New_const1
   | New_const2 of (('a,'b) newtype * 'a * 'b)

   let rec q5_fun (xs:('a,'b) newtype) =
   match xs with
   | New_const1 -> []
   | New_const2 (xs',xa,xb) -> (xa::q5_fun xs')

   let q5a = q5_fun (New_const2 (New_const2 (New_const2 (New_const1,3,true),7,false),5,false))

   (* Question 5a. What is the value of q5a? Add a comment with your
   answer. *)

   (* q5a: int list = [5;7;3] *)
   
   (* Question 5b. What is the type of q5_fun? Add a comment with your
   answer. *)
   
   (* q5_fun: ('a,'b) newtype -> 'a list *)
   
   (* Question 5c. What does q5_fun do? Replace the empty string below
   with your answer. *)

   let answer5c : string = "Extracts the 'a values. From the outer to the inner nestings."
                        
   (* Question 5d. Write a function that takes an input of type
   "(int,float) newtype" and returns a pair of type "int * float"
   where the first element is the sum of all of the integer elements
   appearing in the input data structure and the second element is the
   sum of all of the float elements appearing in the input data
   structure.
   *)

   let rec sum_nt (nt:(int,float) newtype) =
      match nt with
      | New_const1 -> (0, 0.0)
      | New_const2 (nt', i, f) -> 
         let (si, sf) = sum_nt nt' in
            (si + i, sf +. f)

   (* Question 5e. Write an expression of type
   "(int,float) newtype" with 3 integers and 3 floats
   that can be used as input to the function from
   Question 5d. *)

   let q5e = New_const2 (New_const2 (New_const2 (New_const1, 3, 4.0), 2, 5.0), 6, 8.0)

(* Question 6 Imperative Abstract Data Types *)

   module type DICTIONARY =
   sig
      (* An 'a dict is a mapping from strings to 'a.
         We write {k1->v1, k2->v2, ...} for the dictionary which
         maps k1 to v1, k2 to v2, and so forth. *)
      type key = string
      type 'a dict

      (* make an empty dictionary carrying 'a values *)
      val make : unit -> 'a dict

      (* insert a key and value into the dictionary *)
      val insert : 'a dict -> key -> 'a -> unit

      (* Return the value that a key maps to in the dictionary.
         Return None if there is not mapping for the key. *)
      val lookup : 'a dict -> key -> 'a option
   end

   (* Uncomment all of the code below, including the test data and
      complete the implementation of the module below. Use a sorted list
      for the implementation of the dictionary. *)

   module SortedAssocList : DICTIONARY =
   struct
      type key = string
      type 'a dict = (key * 'a) list ref

      let make () : 'a dict = ref []

      let insert (d_:'a dict) (k_:key) (e_:'a) =
         let rec aux (d:'a dict) (k:key) (e:'a) =
            match !d with
            | [] -> d := [(k, e)]
            | (hk, he)::tail ->
               if k = hk then d := (hk, e)::tail
               else if k < hk then d := (k, e)::(hk, he)::tail
               else aux (ref tail) k e;
               d := (hk, he)::(!d)
         in aux d_ k_ e_

      let lookup (d : 'a dict) (k : key) : 'a option =
         let rec aux (l:(key * 'a) list) : 'a option =
         match l with
         | [] -> None
         | (k', x) :: tl ->
            if k = k' then Some x
            else if k < k' then None
            else aux tl in
         aux !d
   end

   let d = SortedAssocList.make()
   let _ = SortedAssocList.insert d "Sam" 22
   let _ = SortedAssocList.insert d "Ada" 19
   let _ = SortedAssocList.insert d "Eric" 24
   let _ = SortedAssocList.lookup d "Sam"
   let _ = SortedAssocList.lookup d "Eric"
   let _ = SortedAssocList.lookup d "Ada"
   let _ = SortedAssocList.lookup d "Christine"

(* Question 7: References *)
