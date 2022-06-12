(* Problems 1 *)

fun merge(x:int list, y : int list) : int list =
	if null (x) then
		if null (y) then []
		else hd y :: merge( x, tl y) 
	else 
		if null (y) then hd x :: merge( tl x, y) 
		else if hd x > hd y then hd y :: merge( x, tl y)
		else if hd x < hd y then hd x :: merge( tl x, y)
		else hd x :: merge( tl x, tl y); (* hd x == hd y *)

(* Problems 2 *)

fun reverse( x : int list ) : int list = 
	let fun rev_help ( a : int list, b : int list) : int list =
			if null ( a ) then b
			else rev_help ( tl a , hd a :: b )
	in rev_help ( x, [] )
	end;

(* Problems 3 *)

fun pi ( a : int, b : int, f : int -> int ) : int = 
	if a = b then f(a) 
	else if a < b then f(a) * pi ( a+1 , b, f)
	else 1

(* Problems 4 *)

fun digits ( x : int ) : int list =
	let fun digits_help ( a : int, b : int list ) : int list =
		if a < 0 then [~1] (* fix it later *)
		else if a < 10 then a :: b
		else digits_help ( a div 10, a mod 10 :: b)
	in digits_help( x, [] ) 
	end;

(* Problems 5 *)

fun list_add ( a : int list ) : int = 
	let fun list_add_help ( x : int list, y : int ) : int = 
			if null (x) then y
			else if null (tl x) then hd x + y
			else list_add_help ( tl x, y + hd x )
	in list_add_help ( a, 0 )
	end;

fun additivePersistence ( x : int ) : int =
	let fun count ( x : int, num : int ) : int =
			if x < 10 then num
			else count ( list_add ( digits(x) ) , num + 1)
	in count ( x, 0)
	end;
	
fun digitalRoot ( x : int ) : int = 
	let val a = digits(x)
	in
	if null (a) then ~1
	else if null (tl a) then x
	else digitalRoot(list_add(a))
	end;
