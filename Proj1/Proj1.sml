(* INTERPRETER FOR A SMALL INTERACTIVE CALCULATOR 
     CIS505/705, K-State, Fall 2019 *)

(* EXCEPTION FOR REPORTING ERRORS *)
exception Error of string

(* THE POWER FUNCTION *)
(* power: int -> int -> int         *)
(* power x n returns x^n for n >= 0 *)
fun power x n =  
   if n = 0
   then 1
   else x * power x (n-1)

(* DICTIONARY OPERATIONS *)
type Key = int

(* initR: Key -> int *)
val initR = [](* MODIFY! *) (*DONE*)

(* updateR: Key -> int -> Dictionary -> Dictionary *)
fun updateR addr new dictionary = 
   ((addr, new)::dictionary) (* MODIFY! *) (*DONE*) 

(* lookupR: Dictionary -> Key -> int *)
fun lookupR [] addr = raise(Error ("Register " ^ Int.toString(addr) ^ " undefined"))
	| lookupR ((addr1, a1)::dictionary) addr = 
		if addr = addr1 then a1	
			else lookupR dictionary addr (* MODIFY! *) (*DONE*)

(* INTERPRETING THE INPUT *)

(* getDigit: char -> int option *)
fun getDigit c = 
   if #"0" <= c andalso c <= #"9"
   then SOME (ord(c) - ord(#"0"))
   else NONE

(* getNumber: char list -> int option -> int option *)
fun getNumber [#"\n"] (SOME acc) = SOME acc
|   getNumber (c :: cs) acc' =
      (case (getDigit c, acc') of
         (SOME d, SOME acc) => getNumber cs (SOME (10 * acc + d))
       | (SOME d, NONE) => getNumber cs (SOME d)
       | _ => NONE)
|   getNumber _ _ = NONE

(* getBinaryOp: char -> (int * int -> int) option *)
fun getBinaryOp #"+" = SOME op+
|   getBinaryOp #"-" = SOME op-
|   getBinaryOp #"*" = SOME op*
|   getBinaryOp #"^" = SOME (fn (v1,v2) => power v1 v2)
|   getBinaryOp _ = NONE

(* THE READ-EVAL-PRINT LOOP *)
(* interpret: Dictionary -> int list -> (int -> int) list -> string -> unit *)
fun interpret registers stackN stackF message = 
    (* FIRST PRINT THE CURRENT MESSAGE *)
  (print (message^"\n? ");
    (* NEXT READ, AND THEN EVAL BASED ON WHAT IS READ *)
   case (TextIO.inputLine TextIO.stdIn) of
     NONE => (raise Error "Error: input a character") 
   | SOME input => 
      (case (getNumber (explode input) NONE) of
        SOME n => 
  (* NUMBER PUT ON STACK *)
         interpret registers (n :: stackN) stackF 
            ((Int.toString n)^" has been put on the number stack")
      | NONE => (case explode input of
          [#"A", #"\n"] =>
  (* FUNCTION IS APPLIED *)
            (case (stackN, stackF) of
               (v :: stack'N, f :: stack'F) =>
                  let val v' = f v in
                     interpret 
                       registers 
                       (v'::stackN)(* MODIFY! *) (*DONE*)
                       stack'F
                       ((Int.toString v')^" has replaced "^
                        (Int.toString v)^" on the top of the number stack "^
                        "(applying the topmost function)")
                   end
             | ([], _) => raise (Error "the number stack is empty")
             | (_, []) => raise (Error "the function stack is empty"))
        | [#"P", #"1", ch, #"\n"] =>
  (* BINARY FUNCTION IS PARTIALLY APPLIED *)
            (case (getBinaryOp ch, stackN) of
               (NONE, _) => raise (Error "a binary operator must follow P1")
             | (_, []) => raise (Error "the number stack is empty")
             | (SOME bop, (v0 :: stack'N)) =>
                  interpret registers stack'N 
                    ((fn v => bop (v0,v)) :: stackF)
                    ((Char.toString ch)^" has been partially applied "^
                     "to "^(Int.toString v0)^" (as 1st argument)"))
        | [#"P", #"2", ch, #"\n"] =>
  (* BINARY FUNCTION IS PARTIALLY APPLIED *)
			(case (getBinaryOp ch, stackN) of
               (NONE, _) => raise (Error "a binary operator must follow P1")
             | (_, []) => raise (Error "the number stack is empty")
             | (SOME bop, (v0 :: stack'N)) =>
                  interpret registers stack'N 
                    ((fn v => bop (v,v0)) :: stackF)
                    ((Char.toString ch)^" has been partially applied "^
                     "to "^(Int.toString v0)^" (as 1st argument)"))
        | [#"M", #"\n"] =>
  (* FUNCTION MAPPED OVER STACK *)
             (case stackF of
               (f1 :: stack'F) =>
                  interpret 
                    registers 
                    (map(fn x => f1 x)(stackN))(* MODIFY! *) (*DONE*)
                    stack'F
                    ("The topmost function has been applied to the "^
                      "whole number stack")
             | [] => raise (Error "the function stack is empty"))
        | [#"R", #"\n"] =>
  (* REGISTER RETRIEVAL *)
			(case stackN of
				(addr :: stack'N)=>
				interpret 
				registers
				((lookupR registers addr)::stack'N)  
				stackF 
				(Int.toString(lookupR registers addr) ^ "has been retrieved from register " ^ Int.toString(addr))
				
				| _ => raise (Error "Input invalid"))
        | [#"S", #"\n"] =>
   (* REGISTER STORE *)
            (case stackN of
              (addr :: new :: stack'N) =>
                interpret 
                  (updateR addr new registers)(* MODIFY! using updateR *) (*DONE*)
                  stack'N 
                  stackF
                  ((Int.toString new)^
                    " has been stored in register "^(Int.toString addr))
           | _ => raise (Error "number stack has at most one element"))
        | [#"C", #"N", #"\n"] =>
   (* COPY NUMBER STACK *)
             (case stackN of
               (v1 :: stack'N) => 
                  interpret 
                     registers 
                     (v1::stackN)(* MODIFY! *) (*DONE*)
                     stackF
                     ((Int.toString v1)^" has been copied on the stack")
             | [] => raise (Error "no number on stack to copy"))
        | [#"C", #"F", #"\n"] =>
   (* COPY FUNCTION STACK *)
             (case stackF of
               (f1 :: stack'F) => 
                  interpret 
                    registers 
                    stackN 
                    (f1::stackF)(* MODIFY! *) (*DONE*)
                    "the top of the function stack has been copied"
             | [] => raise (Error "no function on stack to copy"))
        | [#"D", #"\n"] =>
   (* DISPLAY STACK *)
             interpret registers stackN stackF
                ("content of number stack: "^
                    (concat (map (fn v => (Int.toString v)^" ") stackN)))
        | [#"X", #"\n"] =>
   (* EXIT *)
             (print "Thanks for using the CIS505/705 calculator! Bye\n"; 
                ())
        | _ => raise (Error "input not recognized")))
      handle (Error message) =>
          interpret registers stackN stackF ("ERROR: "^message))

   (* INVOCATION *)
   (* run: unit -> unit *)
fun run () = 
   interpret 
      initR          (* no register has a value *)
      [] []          (* the stacks are both empty *)
      "The CIS505/705 Calculator is ready!"

