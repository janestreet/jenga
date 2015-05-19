(**
   assuming [f] and [default] form a monoid,
   [tree_fold ~f ~default:x = fold_left ~f ~init:x], but [tree_fold] with have a balanced
   [f] application tree, which can help performance.

   For example, compare
   [fold_left ~f:(@) ~init:[]] and [tree_fold ~f:(@) ~init:[]].

   The former is O(n^2) in the total length of the result while the latter is O(n*log(n))
   (assuming non-empty input lists).
*)
val tree_fold : f:('a -> 'a -> 'a) -> default:'a -> 'a list -> 'a
