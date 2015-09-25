(**** LECTURE 1: Algorithms and Recursive Functions ****)
(**** Exercise 1.2 ****)

exception UNDEFINED
fun decode1 a =
  let
      val test1 = a >= 0  andalso a <= 49
      val test2 = a >= 50 andalso a <= 99
  in
      if test1 then a + 2000
      else if test2 then a + 1900
      else raise UNDEFINED
  end;

fun decode2 a =
  let
      fun test1 a = a >= 0  andalso a <= 49
      fun test2 a = a >= 50 andalso a <= 99
  in
      if      test1(a) then a + 2000
      else if test2(a) then a + 1900
      else             raise UNDEFINED
  end;

(* fun decode year = *)
(*   let *)
(*       val test1 = year <= 49 andalso year >= 0 *)
(*       val test2 = year >= 50 andalso year <= 99 *)
(*   in *)
(*       if test1 then year + 2000 *)
(*       else if test2 then year + 1900 *)
(*       else 0 *)
(*   end; *)

  (* if year <= 49 andalso year >= 0 then year + 2000 *)
  (* else if year >=50 andalso year <= 99 then year + 1900 *)
  (* else 0; *)

fun more (year1, year2) =
  decode2 (year1) > decode2 (year2);

fun add (year1, year2) =
  decode2 (year1) + decode2 (year2);
