(**** LECTURE 2: Recursion and Efficiency****)
(**** Exercise 2.1 ****)
(* A boolean-valued function to test whether a number is even *)
fun even n = (n mod 2 = 0);

(* raising to an integer power -- fast and tail-recursive  version *)
fun power (x, n, acc) : real  =
  if n = 0 then acc
  else if even n then power (x * x, n div 2, acc)
  else power(x * x, n div 2, acc * x);
