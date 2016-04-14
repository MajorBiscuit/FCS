(**** LECTURE 4: More on Lists ****)
(**** Exercise 4.2 ****)

(* function that takes a list of integers and returns two lists, the
first consisting of all nonnegative numbers found in the input and
the second consisting of all the negative numbers. *)

fun partition_positive [] = ([], [])
  | partition_positive (x::xs) =
    let fun filter_tuple [] positive_list negative_list = (positive_list, negative_list)
          | filter_tuple (x::xs) positive_list negative_list =
            if (x >= 0) then
                filter_tuple xs (x::(positive_list)) negative_list
            else filter_tuple xs positive_list (x::(negative_list))
     in filter_tuple (x::xs) [] []
     end
