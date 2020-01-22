(* Creates a Binary Tree Datatype *)
datatype ’a BinTree =
	Leaf | 
		Node of ’a BinTree * ’a * ’a BinTree;
(* Height function *)
fun height Leaf = ~1
| height (Node(l,_,r)) = 1 + Int.max(height l, height r);

(* Tree Generated as: *)		
val T1 = Node(Node(Leaf,4,Leaf),
	7,
		Node(Node(Leaf,6,Leaf),8,Node(Leaf,9,Leaf)));
		
(* Generate a BIG tree *)
fun gen_tree n =
	if n = 0
		then Leaf
			else Node(Node(Leaf,3,gen_tree (n-1)),2,Node(Leaf,4,Leaf));		
			
(* FoldT *)	
fun foldt f e Leaf = e
	| foldt f e (Node (l,v,r)) =
		f (foldt f e l, v, foldt f e r);		
		
(* Question 1 *)
fun sum_unbalanced Leaf = 0 | sum_unbalanced (Node(l, x, r)) = 
	if (height l) = (height r) 
		then sum_unbalanced(l) + sum_unbalanced(r)
			else (sum_unbalanced(l) + sum_unbalanced(r) + x);
	
	 
(* Question 2 *)
fun height_sum Leaf = (~1,0) 
| height_sum(Node (L ,n, R)) =
	let val (lh, ls) = height_sum L 
		val (rh, rs) = height_sum R
			val a = if(lh <> rh) then n 
		else 0 
	in (1+ Int.max(lh,rh), a+rs+ls) end;