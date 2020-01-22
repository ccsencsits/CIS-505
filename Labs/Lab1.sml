(*1: Define rotate90 function*)
fun rotate90(x,y) = (~y,x);
(*2: Define rotate function*)
fun rotate n (x,y) = if (n <= 0) then (x,y) else rotate (n-1)(~y,x);
(*3: Define rotate180 function*)
fun rotate180(x,y)= rotate 2(x,y);
(*4: Define interate function*)
fun iterate k f x = if (k=0) then x else iterate(k-1) f(f(x));
(*5: Define rotate270 function*)
fun rotate270 (x,y) = iterate 3 rotate90(x,y);
(*6: compare 270 & itterate*)
(*sml doesnt support equality of functions that is why it gives an error*)