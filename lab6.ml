(* 
  Agenda:
   * scope
   * parameter passing
   * tail recursion
 *)

(* QUESTION 1. Static Scope *)
(* Consider the following program in the Algol-like pseudo-code
   used in the textbook, where f, g, and h are functions (each with
   one parameter) and the function body is inside braces {}.

{ begin
  int x = ...
  int y = ...
  int z = ...
  int f (int u)
      { int a = ...
        int y = ...
        int z = ...
        ...code for f...
      }
  int g (int u)
      { int a = ...
        int x = ...
        int w = ...
        int h (int v)
            { int a = ...
              int b = ...
              int z = ...
              ...code for h...
            }
        ...code for g...
      }
  ...code for main (outer) block...
}                

   Assume that static scope is used.
   (i) List all of the variables that are visible in the body of f,
       and indicate the place where they are declared.
   (ii) Do the same for function g.
   (iii) Do the same for function h.
   (iv) Translate the code above to OCaml giving any values you like
   to the variables.  In the code in the main block, call the
   functions f and g with parameters of your choice.  In the bodies of
   each function, insert print statements that print out the names of
   each variable that is visible along with its value.  
   
   ANSWERS:

    (i)
      u, a, y, and z are all visible and are all declared within f.
      Also x is visible, and it's declared in the main scope.

    (ii)
      u, a, x, and w are all visible and are all declared within g.
      Also y and z are visible and they're declared in the main scope.

    (iii)
      v, a, b, and z are all visible and are all declared within h.
      x and w are are visible and they're declared in g.
      Also y is visible, and it's declared in the main scope.
   
    (iv) See below code
   *)

let main () =
  let x : int = 1 in
  let y : int = 2 in
  let z : int = 3 in
  let f (u:int) =
    let a : int = 4 in
    let y : int = 5 in
    let z : int = 6 in
    print_endline ("u = " ^ string_of_int u);
    print_endline ("a = " ^ string_of_int a);
    print_endline ("y = " ^ string_of_int y);
    print_endline ("z = " ^ string_of_int z);
    print_endline ("x = " ^ string_of_int x)
  in
  let g (u:int) =
    let a : int = 7 in
    let x : int = 8 in
    let w : int = 9 in
    let h (v:int) =
      let a : int = 10 in
      let b : int = 11 in
      let z : int = 12 in
      print_endline ("v = " ^ string_of_int v);
      print_endline ("a = " ^ string_of_int a);
      print_endline ("b = " ^ string_of_int b);
      print_endline ("z = " ^ string_of_int z);
      print_endline ("x = " ^ string_of_int x);
      print_endline ("w = " ^ string_of_int w);
      print_endline ("y = " ^ string_of_int y)
    in
    print_endline ("u = " ^ string_of_int u);
    print_endline ("a = " ^ string_of_int a);
    print_endline ("x = " ^ string_of_int x);
    print_endline ("w = " ^ string_of_int w);
    print_endline ("y = " ^ string_of_int y);
    print_endline ("z = " ^ string_of_int z);
    h(13)
  in
  f(14);
  g(15)



(* QUESTION 2. Dynamic Scope *)
(* Consider the following program (a modified version of the above
   code where the input to each function has type unit, i.e., there
   is no input).

{ begin
  int x = ...
  int y = ...
  int z = ...
  int f ()
      { int a = ...
        int y = ...
        int z = ...
        ...code for f...
      }
  int g ()
      { int a = ...
        int b = ...
        int z = ...
        ...code for g...
      }
  int h ()
      { int a = ...
        int x = ...
        int w = ...
        ...code for h...
      }
  ...code for main (outer) block...
}                

   This time assume dynamic scope.  Given the following calling
   sequences, what variables are visible during execution of the
   last function call in each sequence?  Include with each
   visible variable the name of the block where it is declared
   (main, f, g, or h).  Draw the full activation stack.  In each
   activation record, include the local variables and the
   control link only.  (Note: for this exercise, you don't have
   to include the declarations of functions f, g, and h in the
   activation record for the main block.)

   (i) main calls f; f calls g; g calls h.
   (ii) main calls f; f calls h.
   (iii) main calls g; g calls h; h calls f. 
   
   ANSWERS:

    (i)
      [h: <control link>, a, x, w]
      -> [g: <control link>, a, b, z]
      -> [f: <control link>, a, y, z]
      -> [main: <control link>, x, y, z]

      h has access to:
        a, x, and w from h,
        b and z from g,
        y from f.

    (ii)
      [h: <control link>, a, x, w]
      -> [f: <control link>, a, y, z]
      -> [main: <control link>, x, y, z]

      h has access to:
        a, x, and w from h,
        y and z from f.
  
    (iii)
      [f: <control link>, a, y, z]
      -> [h: <control link>, a, x, w]
      -> [g: <control link>, a, b, z]
      -> [main: <control link>, x, y, z]

      f has access to:
        a, y, and z from f,
        x and w from h,
        b from g.

   *)

(* QUESTION 3. Parameter Passing *)
(* In "pass-by-value-result", also called "call-by-value-result" and
   "copy-in/copy-out", parameters are passed by value, with an added
   twist.  More specifically, suppose a function f with
   pass-by-value-result parameter u is called with actual parameter v.
   The activation record for f will contain a location for formal
   parameter u that is initialized to the R-value of v.  Within the
   body of f, the identifier u is treated as an assignable variable.
   On return from the call to f, the actual parameter v is assigned
   the R-value of u.

   The following pseudo-Algol code illustrates the main properties of
   pass-by-value-result.

   var x : integer;
   x := 0;
   procedure p(value-result y : integer)
     begin
        y := 1;
        x := 0;
     end;
   p(x);

   With pass-by-value-result, the final value of x will be 1: Because
   y is given a new location distinct from x, the assignment to x does
   not change the local value of y.  When the function returns, the
   value of y is assigned to the actual parameter x.  If the parameter
   were passed by reference, then x and y would be aliases and the
   assignment to x would change the value of y to 0.  If the parameter
   were passed by value, the assignment to y in the body of p would
   not change the global variable x and the final value of x would
   also be 0.

   Translate the preceding program fragment into OCaml in a way that
   makes the operations on locations, and the differences between
   L-values and R-values, explicit.  Uncomment the code below and fill
   in your solution.

   *)

let x = ref 0
let p (y' : int ref) : unit =
  let y = ref !y' in
  y := 1;
  x := 0;
  x := !y
let result = p x
let _ = x


(* QUESTION 4. Tail Recursion *)
(* Below are two versions of the factorial function, where the first
   function ("fact") is not tail recursive and the second function
   ("fact_tr") is tail recursive.  Note that "fact_tr" uses an
   auxiliary function with an extra argument to keep track of the
   computation so far.

   Using the same style, transform the function "power" below into a
   tail recursive one.  This function raises the input "a" to the
   power "b" by repeated multiplication.  Use an auxiliary function
   that has a third parameter to represent the result of the
   computation so far.  *)

let rec fact (n:int) =
  if n = 0 then 1 else n*fact(n-1)

let fact_tr (n:int) =
  let rec fact' (n:int) (a:int) =
    if n = 0 then a else fact' (n-1) (n*a) in
  fact' n 1

let rec power (a:int) (b:int) =
  if b = 0 then 1
  else if b = 1 then a
  else a*power a (b-1)

let power_tr (a:int) (b:int) =
  let rec aux (a:int) (b:int) (c:int) =
    if b = 0 then c else aux a (b-1) (c*a) in
  aux a b 1