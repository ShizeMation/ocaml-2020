(* Object-Oriented Programming in OCaml (Real World OCaml Chapter 12):
   Representing Grammars as Classes and Expressions as Objects *)

(* We can represent expressions given by the grammar:

   e ::= num | e + e

   by using objects from a class called "expression".  We begin with
   an abstract class (using the keyword "virtual" in OCaml) called
   "expression".  Although this class has no instances, it lists the
   operations common to all the different kinds of expressions.  These
   operations include a predicate "is_atomic" indicating whether there
   are subexpressions or not, operations to retrieve the left and
   right subexpressions (if the expression is not atomic), and a
   method computing the value of the expression.
 *)

class virtual expression = object
   method virtual is_atomic : bool
   method virtual sub1 : expression option
   method virtual sub2 : expression option
   method virtual sub3 : expression option
   method virtual value : int
   method virtual increment : int -> bool -> unit
   method virtual unvisit : unit -> unit
end

(* Because the grammar has two cases, we have two subclasses of
   "expression", one for numbers, and one for sums.
 *)

class number_exp (n:int) = object(self)
   inherit expression as super
   val mutable number_val = n
   val mutable visited = false
   method is_atomic = true
   method sub1 = None
   method sub2 = None
   method sub3 = None
   method value = number_val
   method increment (i:int) (root:bool) =
      if not visited then
         number_val <- number_val + i;
         visited <- true;
      if root then
         self#unvisit()
   method unvisit () =
      visited <- false
end               

class sum_exp (e1:expression) (e2:expression) = object(self)
   inherit expression as super
   val mutable left_exp = e1
   val mutable right_exp = e2
   val mutable visited = false
   method is_atomic = false
   method sub1 = Some left_exp
   method sub2 = Some right_exp
   method sub3 = None
   method value = left_exp#value + right_exp#value
   method increment (i:int) (root:bool) =
      if not visited then
         left_exp#increment i false;
         right_exp#increment i false;
         visited <- true;
      if root then
         self#unvisit()
   method unvisit () =
      left_exp#unvisit();
      right_exp#unvisit();
      visited <- false
end

(* QUESTION 1. Product Class and Method Calls *)
(* 1(a). Extend this class hierarchy by writing a "prod_exp" class to
   represent product expressions of the form:

   e ::= ... | e * e
 *)

class prod_exp (e1:expression) (e2:expression) = object(self)
   inherit expression as super
   val mutable left_exp = e1
   val mutable right_exp = e2
   val mutable visited = false
   method is_atomic = false
   method sub1 = Some left_exp
   method sub2 = Some right_exp
   method sub3 = None
   method value = left_exp#value * right_exp#value
   method increment (i:int) (root:bool) =
      if not visited then
         left_exp#increment i false;
         right_exp#increment i false;
         visited <- true;
      if root then
         self#unvisit()
   method unvisit () =
      left_exp#unvisit();
      right_exp#unvisit();
      visited <- false
end

(* 1(b). Create objects representing the following expressions:
   - An expression "a" representing the number 3
   - An expression "b" representing the number 0
   - An expression "c" representing the number 5
   - An expression "d" representing the sum of "a" and "b"
   - An expression "e" representing the product of "d" and "c"
   Then send the message "value" to "e".
   Note that "e" represents the expression (3+0)*5.

   To answer 1(b), uncomment this code and fill it in:
   *)

let a = new number_exp 3
let b = new number_exp 0
let c = new number_exp 5
let d = new sum_exp a b
let e = new prod_exp d c

(* QUESTION 2. Unary Expressions *)
(* Extend the class hierarchy further by writing a "square_exp".
   The expression below written e^2 means "e squared":

   e ::= ... | e^2

   Changes will be required to the "expression" interface, so you will
   need to reimplement all the classes from above with these changes.
   Try to make as few changes as possible to the program. *)

class square_exp (e:expression) = object(self)
   inherit expression as super
   val mutable exp = e
   val mutable visited = false
   method is_atomic = false
   method sub1 = Some exp
   method sub2 = None
   method sub3 = None
   method value = exp#value * exp#value
   method increment (i:int) (root:bool) =
      if not visited then
         exp#increment i false;
         visited <- true;
      if root then
         self#unvisit()
   method unvisit () =
      exp#unvisit();
      visited <- false
end

(* QUESTION 3. Ternary Expressions and More Method Calls *)
(* 3(a). Extend this class heirarchy by writing a "cond_exp" class to
   represent conditionals of the form

   e ::= ... | e?e:e

   In a conditional expression a?b:c, evaluate "a" and if the value is
   not 0, then return the value of "b".  If the value of "a" is 0,
   then return the value of "c".

   Again, try to make as few changes as possible to the program.  If
   necessary, redesign the class hierarchy you created for Question 2
   so that it handles both unary and ternary expressions.
 *)

class cond_exp (e1:expression) (e2:expression) (e3:expression) = object(self)
   inherit expression as super
   val mutable c_exp = e1
   val mutable t_exp = e2
   val mutable f_exp = e3
   val mutable visited = false
   method is_atomic = false
   method sub1 = Some c_exp
   method sub2 = Some t_exp
   method sub3 = Some f_exp
   method value =
      if c_exp#value = 0 then f_exp#value else t_exp#value
   method increment (i:int) (root:bool) =
      if not visited then
         c_exp#increment i false;
         t_exp#increment i false;
         f_exp#increment i false;
         visited <- true;
      if root then
         self#unvisit()
   method unvisit () =
      c_exp#unvisit();
      t_exp#unvisit();
      f_exp#unvisit();
      visited <- false
end
   
(* 3(b). Re-create all the objects a,b,c,d,e above and create new
   objects:
   - An expression "f" representing the square of "c"
   - An expression "g" representing the conditional b?e:f
   Then send the message "value" to "g".
 *)

let f = new square_exp c
let g = new cond_exp b e f

(* 3(c) Enter the following expressions (for a,b,c,d,e,f,g) into OCaml
   so that you can see what is printed by the OCaml interpreter.  In
   each case, the type of the expression will be printed. Note that
   they are not all the same.

   Then enter the expression defining the value of "e_list".  Note
   that this is a list containing elements of type "expression", which
   is a different type than the ones printed out for a,b,c,d,e,f,g.
   Explain why these elements are all allowed to have more than one
   type in OCaml.

   To answer 3(c), uncomment this code and execute it.

let _ = a
let _ = b
let _ = c
let _ = d
let _ = e
let _ = f
let _ = g
let e_list : expression list = [a;b;c;d;e;f;g]

   ANSWER:

   Because the different class types that we've defined are
   inherited from the 'expression' super class. Inherited classes
   are allowed to take the place of the super class.

 *)

(* QUESTION 4. Redesign the entire hierarchy again, so that it
   includes a new operation that takes one argument (x:int) and
   modifies an expression object so that all of its leaves are
   incremented by the value of x.  (The leaves of an expression are
   all the subexpressions belonging to the "number_exp" class.)  This
   operation should not return a new instance of an "expression".  It
   should modify the instances that it is applied to.

   Re-create all the objects a,b,c,d,e,f,g again.  Then send the
   message "value" to "g".  Then apply the new operation with any
   argument value greater than 0.  Then send the message "value" to
   "g". The new value should be different than the original one.
   Verify that your implementation gives the expected new value.  *)

let a = new number_exp 3
let b = new number_exp 0
let c = new number_exp 5
let d = new sum_exp a b
let e = new prod_exp d c
let f = new square_exp c
let g = new cond_exp b e f

let a' = a#value
let b' = b#value
let c' = c#value
let d' = d#value
let e' = e#value
let f' = f#value
let g' = g#value

let _ = g#increment 1 true

let a'' = a#value
let b'' = b#value
let c'' = c#value
let d'' = d#value
let e'' = e#value
let f'' = f#value
let g'' = g#value
