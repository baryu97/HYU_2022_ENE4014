(* Problem 3 *)
type name = string
datatype RSP =
 ROCK
 | SCISSORS
 | PAPER
datatype 'a strategy = Cons of 'a * (unit -> 'a strategy)
datatype tournament =
 PLAYER of name * (RSP strategy ref)
 | MATCH of tournament * tournament

fun onlyOne(one:RSP) = Cons(one, fn() => onlyOne(one))
fun alterTwo(one:RSP, two:RSP) = Cons(one, fn() =>
alterTwo(two, one))
fun alterThree(one:RSP, two:RSP, three:RSP) = Cons(one, fn()
=> alterThree(two, three, one))

val r = onlyOne(ROCK)
val s = onlyOne(SCISSORS)
val p = onlyOne(PAPER)
val rp = alterTwo(ROCK, PAPER)
val sr = alterTwo(SCISSORS, ROCK)
val ps = alterTwo(PAPER, SCISSORS)
val srp = alterThree(SCISSORS, ROCK, PAPER)

fun next(strategyRef) =
 let val Cons(rsp, func) = !strategyRef in
 strategyRef := func();
 rsp
 end

fun whosWinner(t) : tournament =  
	let fun eval (x) = 
		case x of PLAYER (a,b) => b
	fun win_help (a,b) = 
		let val a1 = eval(a); val b1 = eval(b) in
		case next(a1) of ROCK => (case next(b1) of ROCK => win_help(a,b)
							| SCISSORS =>  a
							| PAPER => b)
				|SCISSORS => (case next(b1) of ROCK => b
								| SCISSORS => win_help(a,b)
								| PAPER => a)
				|PAPER => (case next(b1) of ROCK => a
								| SCISSORS => b
								| PAPER => win_help(a,b))
		end
				
	in
	case t of PLAYER (a,b) => PLAYER (a,b)
		| MATCH (a,b) => win_help(whosWinner(a),whosWinner(b))
	end
(* Problem 2 *)

datatype pattern =  Wildcard
			| Variable of string 
			| UnitP
			| ConstP of int 
			| TupleP of pattern list
			| ConstructorP of string * pattern
datatype valu =      Const of int 
			| Unit 
			| Tuple of valu list
			| Constructor of string * valu

fun unzip xs =
	case xs of [] => []
			| x::xs' => x @ (unzip xs')

fun match (va, pat) : (string * valu) list option = 
	   case pat of Wildcard => SOME []
			| Variable s => SOME [(s,va)]
			| UnitP => (case va of Unit => SOME [] | Const _ => NONE | Tuple _ => NONE | Constructor (_,_) => NONE)
			| ConstP i => (case va of Unit => NONE | Const a => if i=a then SOME [] else NONE | Tuple _ => NONE | Constructor (_,_) => NONE)
			(* | TupleP [] => (case va of Unit => NONE | Const _ => NONE | Tuple _ => NONE | Constructor (_,_) => NONE) *)
			| TupleP ps => (case va of Unit => NONE | Const _ => NONE | Constructor (_,_) => NONE | 
						Tuple vs => if List.length ps = List.length vs then 
									let val x = List.map match (ListPair.zip(vs,ps))
									in if List.exists (fn n => n=NONE) x then NONE
									   else SOME (unzip(List.map valOf x)) end
								else NONE)
			| ConstructorP (s1,p) => (case va of Unit => NONE | Const _ => NONE |
							 Tuple _ => NONE | Constructor (s2,v) => if s1=s2 then match(v,p) else NONE)

(* Problem 1 *)
fun fold f acc xs =
case xs of
[] => acc
| x::xs' => fold f (f(acc,x)) xs'

fun check_pat pat = 
	let fun fold1 (strs,pa) =
		   case pa of Wildcard => strs
				| Variable str => str::strs
				| UnitP => strs
				| ConstP _ => strs
				| TupleP pats => fold fold1 strs pats
				| ConstructorP ( _, p ) => fold1 (strs,p);
		fun single_list (xs : string list) = 
		    case xs of [] => true
				| x :: xs' => if List.exists (fn n => n=x) xs' then false else single_list xs'
	in single_list (fold1([], pat))
	end;
