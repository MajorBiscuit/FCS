(**** LECTURE 1: Algorithms and Recursive Functions ****)
(**** Exercise 1.5 ****)

fun nadd (x, n) =
  if n = 0 then 0.0
  else if n =1 then x
  else x + nadd (x, n - 1);
