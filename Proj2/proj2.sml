(* ABSTRACT SYNTAX

type Id = string
type Num = int

datatype Exp =
  NumE of Num
| GetE of Left
| AddE of Exp * Exp
| SubE of Exp * Exp
| MulE of Exp * Exp
and Left =
  LocOf of Id * Exp list

datatype Comm =
  SkipC
| SeqC of Comm * Comm
| IfC of Exp * Comm * Comm
| WhileC of Exp * Comm
| AssignC of Left * Exp
| OutputC of Exp
| InputC of Left

type Decl = (Id * Num list) list

datatype Prog = 
  ProgP of Decl * Comm
| ErrorP of string   (* to report errors *)

*)

(* EXCEPTIONS *)

exception NotDeclared of string
exception BoundsError of string
exception ArrayDiscrepancy of string
exception InputError of string 
exception OutOfBoundsError of string

(* ENVIRONMENTS and STORES *)

type Loc = int    (* locations in stores *)

type Value = int    (* values of expressions *)

type Env = Id -> Loc * Num list 
  (* associates identifiers with locations
      and with their dimensions (null if integers) *)

(* InitEnv: Env *)
fun InitEnv id = raise (NotDeclared id)

(* EnvInsert: Id -> Loc * Num list -> Env -> Env *)
fun EnvInsert id0 (loc,dims) env = 
   fn id => if id = id0 then (loc,dims) else env id

(* EnvLookup: Env -> Id -> Loc * Num list *)
fun EnvLookup env id = env id

(* Stores *)

type Store = Loc -> Value

(* InitSto: Store *)
fun InitSto loc = 0 (* all locations are initially zero *)

(* StoUpdate: Loc -> Value -> Store -> Store)  *)
fun StoUpdate loc0 v sto =
   fn loc => if loc = loc0 then v else sto loc

(* StoLookup: Store -> Loc -> Value *)
fun StoLookup sto loc = sto loc

(* INDEX CALCULATION *)

(* calculate_displacement: Id -> int list -> int list -> int *)
fun calculate_displacement id indices bounds = case (indices, bounds) of
   ([],[]) => 0
 | ((index1 :: indices'), (bound1 :: bounds')) =>
           index1 (* CHANGE #9 *)
 | _ => raise (ArrayDiscrepancy id)

(* EVALUATION OF EXPRESSIONS
     ExpEval: Exp -> Env -> Store -> Val
*)

"}[p-0\><
fun ExpEval (NumE n) _ _ = n (* CHANGE #1: *) (* DONE *)
|   ExpEval (GetE lhs) env sto = StoLookup sto (LeftEval lhs env sto)
|   ExpEval (AddE(exp1,exp2)) env sto =
      let val v1 = ExpEval exp1 env sto
          val v2 = ExpEval exp2 env sto
       in v1 + v2
      end
|   ExpEval (SubE(exp1,exp2)) env sto =
      let val v1 = ExpEval exp1 env sto
          val v2 = ExpEval exp2 env sto
       in v1 - v2
      end
|   ExpEval (MulE(exp1,exp2)) env sto =
      let val v1 = ExpEval exp1 env sto
          val v2 = ExpEval exp2 env sto
       in v1 * v2
      end
(* LeftEval: Left -> Env -> Store -> Loc   *)
and LeftEval (LocOf (id, exps)) env sto = let
        val (loc,bounds) = EnvLookup env id
        val indices = map (fn exp => ExpEval exp env sto) exps
        val displ = calculate_displacement id indices bounds
       in loc + displ
      end

(* PROCESSING OF DECLARATIONS 
     DeclExec: Decl -> Env * int -> Env * int
*)

fun DeclExec [] (env,next) = (env,next)
|   DeclExec ((id, bounds) :: decl) (env,next) = 
      if List.all (fn bound => bound > 0) bounds
      then DeclExec decl
              (EnvInsert id (next, bounds) env,
               next + (foldr op* 1 bounds))
      else raise (BoundsError id)

(* EXECUTION OF COMMANDS *)

type InputStream = Num list
type OutputStream = Value list
type RunTimeState = Store * InputStream * OutputStream

(*
CommExec: Comm -> Env -> RunTimeState -> RunTimeState
*)

fun CommExec SkipC env state = state
|   CommExec (SeqC(cmd1,cmd2)) env state = (* CHANGE #2 *) (* DONE *)
      let val state1 = CommExec cmd1 env state
          val state2 = CommExec cmd2 env state1
       in state2 end
|   CommExec (IfC(exp,cmd1,cmd2)) env (state as(sto, inp, outp))  = (* CHANGE #7 *) (* DONE *)
        if ExpEval exp env sto > 0
          then (CommExec cmd1 env state)
          else (CommExec cmd2 env state)
|   CommExec (WhileC(exp,cmd)) env (sto, inp, outp) = (* CHANGE #8: *) (* DONE *)
		if(ExpEval exp env sto) > 0
			then let val (sto1, inp1, outp1) = CommExec cmd env (sto, inp, outp)
				in CommExec(WhileC(exp, cmd)) env (sto1, inp1, outp1) end	
			else CommExec(SkipC) env (sto, inp, outp)
|   CommExec (AssignC(lhs, rhs)) env (sto, inp, outp) = (* CHANGE #3: *)(* DONE *)
          ((StoUpdate (LeftEval lhs env sto) (ExpEval rhs env sto) sto, inp, outp))
|   CommExec (OutputC exp) env (sto, inp, outp) =
      let val v = ExpEval exp env sto
       in (sto, inp, (v::outp))   (* we eventually reverse the order *)
      end
|   CommExec (InputC lhs) env (sto,inp,outp) = (* CHANGE #6 *) (* DONE *)
	case(inp) of 
		[] => raise InputError("Invalid Input")
		| (x::xs) => ((StoUpdate(LeftEval lhs env sto)x sto), xs, outp)
(* RUNNING THE PROGRAM *)

fun ProgRun (ProgP(decl,comm)) inp =
       let val (env,_) = DeclExec decl (InitEnv, 0)
           val (_,_,outp) = CommExec comm  env (InitSto, inp, [])
         in rev outp
        end
|   ProgRun(ErrorP s) _ = (print ("*** syntax error: "^s^"\n"); [0])

fun Interpret prog inp = ProgRun (parse prog) inp
   handle (* CHANGE #5 *)
      (NotDeclared x) =>
         (print ("*** error: "^x^" used but not declared\n"); [0])
    | (BoundsError x) =>
         (print ("*** error: "^x^" declared with a zero bound\n"); [0])
    | (ArrayDiscrepancy x) =>
         (print ("*** error: "^x^" not used with same dimensions as declared\n"); [0])
   | (InputError x) =>
		(print("*** error: "^x^" in an invalid input\n"); [0])
   | (OutOfBoundsError x) =>
		(print("*** error: "^x^" is out of bounds\n"); [0])