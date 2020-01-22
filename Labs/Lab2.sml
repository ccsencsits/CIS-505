(*Question 1: create a Function, given list returns string of numbers as "1 2 3 "*)
fun show_numbers(ls) =
			case ls of	
				nil => " " | 
					(n::ns) => Int.toString (n) ^ " " ^ show_numbers(ns);(*I feel the cases are right just the concatenation and recursive calls are not*)
					
(*Question 2: Create function, sqrt_nonneg, given list of real # returns square roots*)
fun sqrt_nonneg nil = [] | (*do [] so you can get a list rathr th*)
	sqrt_nonneg (n::ns) = if (n < 0.0) then 
		sqrt_nonneg(ns)
			else Math.sqrt(n)::sqrt_nonneg(ns);
(*Question 3: Ceate function find_each_max that given a list of integer lists, returns the maxima of each list. *)
fun find_max nil = 0 
	| find_max [x] = x
		| find_max (x::xs) = Int.max(x, find_max xs);(*Finds max of a list but not list of lists*)
		
fun find_each_max nil = [0] 
		| find_each_max (x::xs) = find_max(0, x) find_each_max xs;
(*Question 4: Use map, filter, and foldr to re write above functions*)

fun show_numbers sn = (String.concatWith " " (map Int.toString sn));
fun sqrt_nonneg nn = map Math.sqrt(nn);(*nan for those negative numbers, how to skip them??*)
fun find_each_max em = ;