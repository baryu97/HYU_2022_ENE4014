(* Problems 1 *)

datatype expr = NUM of int
 | PLUS of expr * expr
 | MINUS of expr * expr ;

 datatype formula = TRUE
 | FALSE
 | NOT of formula
 | ANDALSO of formula * formula
 | ORELSE of formula * formula
 | IMPLY of formula * formula
 | LESS of expr * expr ;

fun int_exp (exp : expr) : int =
	case exp of  NUM a => a
		|	PLUS (a,b) => int_exp (a) + int_exp(b)
		|	MINUS (a,b) => int_exp (a) - int_exp(b);
fun eval (fml : formula) : bool = 
	case fml of   TRUE => true
		|	FALSE => false
		|	NOT a => not (eval (a))
		|	ANDALSO (a,b) => eval (a) andalso eval (b)
		|	ORELSE (a,b) => eval (a) orelse eval (b)
		|	IMPLY (a,b) => if eval (a) then eval (b) else true
		|	LESS (a,b) => int_exp (a) < int_exp (b);

(* Problems 2 *)

type name = string ;
 datatype metro = STATION of name
 | AREA of name * metro
 | CONNECT of metro * metro ;

fun inList ( a : string, b : string list) : bool =
	if null b then false
	else if a = hd b then true
	else inList( a, tl b);

fun checkMetro (met : metro) : bool =
	let fun checkList ( a : string list, met1 : metro) : bool =
			case met1 of STATION x => inList ( x, a )
				|	 AREA ( x, y ) => checkList( x :: a, y)
				|	 CONNECT ( x, y ) => checkList ( a, x ) andalso checkList ( a, y )
	in
		checkList( [], met)
	end;

(* Problems 3 *)

datatype 'a lazyList = nullList
 			| cons of 'a * (unit -> 'a lazyList) ;

fun eval_lzl a =
	case a of nullList => []
		|   cons (x,y) => x :: eval_lzl(y());

fun seq(first, last)= 
	if first = last then cons(first, fn () => nullList)
	else cons(first, fn () => seq(first+1,last) );

fun infSeq(first) =
	cons(first, fn () => infSeq(first+1) );

fun firstN(lazyListVal,n) =
	if n = 1 then case lazyListVal of nullList => [] | cons(a,_) => [a]
	else  case lazyListVal of nullList => [] | cons(a,b) => a :: firstN(b(),n-1) ;

fun Nth(lazyListVal, n) = 
	if n = 1 then case lazyListVal of nullList => NONE | cons(a,_) => SOME a
	else case lazyListVal of nullList => NONE | cons(a,b) => Nth(b(),n-1);

fun filterMultiples(lazyListVal,n) =
	case lazyListVal of nullList => nullList
			|cons(a,b) => if a mod n = 0 then filterMultiples( b(), n) 
					else cons(a,fn () => filterMultiples( b(), n)) ;

fun primes() = 
	let fun sieve(lazyListVal) =
	case lazyListVal of nullList => nullList |	cons(a,b) => cons(a,fn () =>sieve(filterMultiples(b(),a)));
	in sieve(infSeq(2)) end;