(**** LECTURE 3: Lists ****)
(**** Exercise 3.x ****)
fun rsum [] = 0
  | rsum (x::xs) = x + rsum xs;

fun isum ([], acc) = acc
  | isum (x::xs, acc) = isum (xs, x + acc);

fun trsum x =
  let
      fun isum ([], acc) = acc
        | isum (x::xs, acc) = isum (xs, x + acc);
  in
      isum (x, 0)
  end;

fun last_list l = hd (rev (l));

fun even_elements (x::xs, index, acc) =
  if (index + 1) mod 2 = 0 then even_elements (xs, index + 1, acc@[x])
  else even_elements (xs, index + 1, acc)
  | even_elements ([], index, acc) = acc;
