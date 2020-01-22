/*

=== Parser and Interpreter,
       for the language given in Project 5, CIS505/705, 
       Kansas State University, Fall 2019.
   Torben Amtoft

This is a skeleton, with several places where code needs to be inserted.
Each such place is marked with "CHANGE #k" where k is a number
    (perhaps with a suffix).

#1: how to solve a query involving inequality of two numbers

#2a,#2b,#2c: how to evaluate sums, differences, and products

At this point, you can evaluate simple ground queries like
           4 + 5 * 6 = 34, etc.

#3: how to handle the "some" construct
  #3a: how to generate all values from a given interval
  #3b: how to solve an existential query;
          this should involve generating all possible instances
            of the quantified variable.
  #3c: how to evaluate variables (observe: there is no environment)

At this point, you can handle the first many queries in the question text.

#4: how to parse disjunction
      (hint: take inspiration from how to parse conjuction)

#5: how to handle universal quantifiers
      hint: by DeMorgan, forall x.P is the same as not(exists x.not P)

*/

/* ask(Tokens,Size)
   INPUT: Tokens is a list representing a condition with quantified variables
          Size is the size of the universe considered for each variable
   OUTPUT: Any instantiation of the variables mentioned existentially
        (without a surrounding universal quantifier) 
      which satisfies the condition represented by Tokens
*/

ask(Tokens,Size) :- 
  parse(Tokens,Tree), solve(Tree,Size).

/* parse(Tks,C)
   INPUT: Tks is a list of tokens
   OUTPUT: if Tks forms a boolean expression
           then C is instantiated to a condition tree representing that boolean expression
*/

parse(Tks,C) :- parseBE(Tks,[],C).

/* parseBE(Tks,R,C)
   INPUT: Tks is a list of tokens
   OUTPUT: if a prefix of Tks forms a boolean expression
           then R is the remaining tokens
           and C is instantiated to a condition tree representing that boolean expression
*/
parseBE(Tks,R,C) :- parseBT(Tks,R,C).
parseBE(Tks,R,or(C1, C2)) :- parseBT(Tks,[Or | R1], C1), already(Or, #), parseBE(R1, R, C2).
/*  CHANGE #4  Make rule to parse disjunction  */


/* parseBT(Tks,R,C)
   INPUT: Tks is a list of tokens
   OUTPUT: if a prefix of Tks forms a boolean term
           then R is the remaining tokens
           and C is instantiated to a condition tree representing that boolean term
*/
parseBT(Tks,R,C) :- parseBF(Tks,R,C).
parseBT(Tks,R,and(C1,C2)) :- parseBF(Tks,[And | R1],C1), already(And,&), parseBT(R1,R,C2).

/* parseBF(Tks,R,C)
   INPUT: Tks is a list of tokens
   OUTPUT: if a prefix of Tks forms a boolean factor
           then R is the remaining tokens
           and C is instantiated to a condition tree representing that boolean factor
*/
parseBF(Tks,R,greater(E1,E2)) :- parseE(Tks,[Greater | R1],E1), already(Greater,>), parseE(R1,R,E2).
parseBF(Tks,R,geq(E1,E2)) :- parseE(Tks,[Geq | R1],E1), already(Geq,>=), parseE(R1,R,E2).
parseBF(Tks,R,equal(E1,E2)) :- parseE(Tks,[Eq | R1],E1), already(Eq,=), parseE(R1,R,E2).
parseBF([Q | Tks],R, existential(V,C)) :- already(Q, exists(V)), parseBF(Tks,R,C).
parseBF([Q | Tks],R, universal(V,C)) :- already(Q, forall(V)), parseBF(Tks,R,C).
parseBF([Tks | R], R, C) :- compound(Tks), parseBE(Tks, [], C).

/* parseE(Tks,R,T)
   INPUT: Tks is a list of tokens
   OUTPUT: if a prefix of Tks forms an expression
           then R is the remaining tokens
           and T is instantiated to an expression tree representing that expression
*/
parseE(Tks,R,E) :- parseT(Tks,R,E).
parseE(Tks,R,add(E1,E2)) :- parseT(Tks,[Plus | R1],E1), already(Plus,+), parseE(R1,R,E2).
parseE(Tks,R,subtract(E1,E2)) :- parseT(Tks,[Minus | R1],E1), already(Minus,-), parseE(R1,R,E2).

/* parseT(Tks,R,T)
   INPUT: Tks is a list of tokens
   OUTPUT: if a prefix of Tks forms a term
           then R is the remaining tokens
           and T is instantiated to an expression tree representing that term
*/
parseT(Tks,R,E) :- parseF(Tks,R,E).
parseT(Tks,R,mult(E1,E2)) :- parseF(Tks,[Times | R1],E1), already(Times,*), parseT(R1,R,E2).

/* parseF(Tks,R,T)
   INPUT: Tks is a list of tokens
   OUTPUT: if a prefix of Tks forms a factor
           then R is the remaining tokens
           and T is instantiated to an expression tree representing that factor
*/
parseF([C | Tks], Tks, constant(C)) :- number(C).
parseF([Tks | R], R, E) :- compound(Tks), parseE(Tks, [], E).
parseF([X | Tks], Tks, variable(X)) :- var(X).

/* already(X,C)
   INPUT: C is instantiated to a non-variable
   OUTPUT: is true if X is already instantiated to something 
      that matches that non-variable
*/
already(X,C) :- not(var(X)), X = C. 

/* evaluate(E,V)
   INPUT: E is a fully instantiated expression tree
   OUTPUT: V is instantiated to the value of evaluating E
*/
evaluate(constant(N),N).
/*  CHANGE #3c make rule for variable(N)     */
evaluate(variable(N),N).
/*  CHANGE #2a make rule for add(E1,E2)     */
evaluate(add(E1,E2), N) :- evaluate(E1,N1), evaluate(E2, N2), N is N1 + N2.
/*  CHANGE #2b make rule for subtract(E1,E2)     */
evaluate(subtract(E1,E2), N) :- evaluate(E1,N1), evaluate(E2, N2), N is N1 - N2.
/*  CHANGE #2c make rule for mult(E1,E2)    */
evaluate(mult(E1,E2), N) :- evaluate(E1,N1), evaluate(E2, N2), N is N1 * N2.

/* solve(C,Sz)
   INPUT: C is a condition tree without free variables
          Sz is the size of the universe considered for each variable
   OUTPUT: succeeds (perhaps several times) iff C evaluates to true,
         together with the corresponding instantiations of the 
         existentially quantified variables 
               (that are not surrounded by a universal quantifier)
*/
solve(geq(E1,E2),_) :- evaluate(E1,V1), evaluate(E2,V2), V1 >= V2.
solve(equal(E1,E2),_) :- evaluate(E1,V1), evaluate(E2,V2), V1 = V2.
/*  CHANGE #1  make rule(s) for greater(E1,E2)    */
solve(greater(E1,E2),_) :- evaluate(E1,V1), evaluate(E2,V2), V1 > V2.
solve(and(C1,C2),Sz) :- solve(C1,Sz), solve(C2,Sz).
solve(or(C1,_),Sz) :- solve(C1,Sz).
solve(or(_,C2),Sz) :- solve(C2,Sz). 
/*  CHANGE #3b make rule(s) for existential(V,C)   */
 solve(existential(V,C), U) :- gen_instances(V,0,U-1), solve(C, U).
/*  CHANGE #5  make rule(s) for universal(V,C)   */
 solve(universal(V,C), U) :- not(solve(existential(V,not(C)),U)).

/* gen_instances(V,L,U)
    INPUT: L,U are integers; V a variable
    OUTPUT: V is instantiated to 
         all values in the interval [L..U]
*/
gen_instances(L,L,U) :- L =< U.
gen_instances(V,L,U) :- K is L + 1, K =< U, gen_instances(V, K, U).
/*  CHANGE #3a make rule that generates the numbers from L+1 up to U */

