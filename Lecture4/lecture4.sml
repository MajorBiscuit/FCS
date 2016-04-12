(**** LECTURE 4: More Lists ****)

(** take and drop **)

(*not iterative, but it doesn't matter!*)
fun take ([], _)    = []
  | take (x::xs, i) = if i>0 then x :: take(xs, i-1)
                      else  [];

fun drop ([], _)    = []
  | drop (x::xs, i) = if i>0 then drop (xs, i-1)
                             else x::xs;


(** zip and unzip **)

fun zip (x::xs,y::ys) = (x,y) :: zip(xs,ys)
  | zip _             = [];

fun unzip [] = ([],[])
  | unzip((x,y)::pairs) =
        let val (xs,ys) = unzip pairs
        in  (x::xs, y::ys)  end;

fun revUnzip ([], xs, ys)           = (xs,ys)
  | revUnzip ((x,y)::pairs, xs, ys) =
    revUnzip(pairs, x::xs, y::ys);


(** simple "set" operations -- equality type variables **)

(*membership in a list*)
fun member(x, [])   =  false
  | member(x, y::l) = (x=y) orelse member(x,l);

(*insert the list of xs into the ys, adding no duplicates*)
fun union([],ys) = ys
  | union(x::xs, ys) = union(xs, if member(x,ys) then ys else x::ys);

fun inter([],ys) = []
  | inter(x::xs, ys) =
        if member(x,ys) then x::inter(xs, ys)
                        else    inter(xs, ys);


(** making change (with unlimited supplies of coins) **)

(*Make change for the amount*)
fun change1 (till, 0) = []
  | change1 (c::till, amt) =
      if amt<c then change1(till, amt)
      else c :: change1(c::till, amt-c);

(*coin values given in decreasing order*)
(* change1 ([50,20,10,5,2,1], 99); *)

(*Return ALL ways of making change*)
fun change4 (till, 0)    = [[]]
  | change4 ([], amt)    = []
  | change4 (c::till, amt) =
      if amt<c then change4(till, amt)
      else
	let fun allc [] = []
	      | allc (cs::rest) = (c::cs) :: allc rest
	in allc (change4(c::till, amt-c)) @
           change4(till, amt)
        end;

(* change4 ([50,20,5,2], 99); *)
(* change4 ([50,20,5,2], 150); *)
(* change4 ([5,2], 6); *)
(* change4 ([5,2], 11); *)

(*Return ALL ways of making change: another version*)
fun change5 (till,    0, chg, chgs)   = chg::chgs
  | change5 ([], amt, chg, chgs)      = chgs
  | change5 (c::till, amt, chg, chgs) =
      if amt<0 then chgs
      else change5 (c::till, amt-c, c::chg,
		  change5 (till, amt, chg, chgs));

(* change5 ([50,20,5,2], 99, [], []); *)
(* change5 ([50,20,5,2], 150, [], []); *)
(* change5 ([5,2], 6, [], []); *)
(* change5 ([5,2], 11, [], []); *)
(* change5 ([200,100,50,20,10,5,2,1], 1500, [], []); *)
