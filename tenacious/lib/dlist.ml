open Core.Std
type 'a t = 'a list -> 'a list
let empty = fun l -> l
let singleton x = fun l -> x :: l
let of_list x = fun l -> x @ l
let (@) a b = fun l -> a (b l)
let to_list x = x []
let concat = List.fold_left ~f:(@) ~init:empty

TEST = to_list empty = []
TEST = to_list (singleton 1) = [1]
TEST = to_list (of_list [1;2;3]) = [1;2;3]
TEST = to_list (of_list [1;2;3] @ of_list [4;5;6]) = [1;2;3;4;5;6]
TEST = to_list (concat (List.map ~f:of_list [[1;2];[3;4];[5;6]])) = [1;2;3;4;5;6]
