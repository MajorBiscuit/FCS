(* Remove duplicates from list *)
(* Outer function progresses through each element of the list *)
fun remove_duplicates [] = []
  | remove_duplicates (x::xs) =
    (* Inner function checks for duplicates of the current first item in the
    whole list *)
    let fun remove (x,[]) = []
          | remove (x, y::ys) =
            if x = y then remove(x,ys)
            else y::remove(x,ys)
    in
        x::remove_duplicates(remove(x,xs))
    end

(* Remove duplicates from sorted list *)
fun remove_dups (head :: (tail_list as t :: _)) =
  if head = t then
      remove_dups tail_list
  else
      head :: remove_dups tail_list
|   remove_dups tail_list = tail_list;
