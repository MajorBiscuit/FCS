(**** LECTURE 1: Algorithms and Recursive Functions ****)
(**** Exercise 1.6 ****)

fun gamma (constant, n) : real =
  if n = 0 then constant
  else 1.0 / (gamma (constant, n - 1) - 1.0);
