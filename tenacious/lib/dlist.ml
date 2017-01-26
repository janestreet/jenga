open Core
open! Int.Replace_polymorphic_compare

type 'a t = 'a list -> 'a list
let empty = fun l -> l
let singleton x = fun l -> x :: l
let of_list x = fun l -> x @ l
let (@) a b = fun l -> a (b l)
let to_list x = x []
let concat = List.fold_left ~f:(@) ~init:empty

let%test_unit _ = [%test_result: int list] (to_list empty) ~expect:[]
let%test_unit _ = [%test_result: int list] (to_list (singleton 1)) ~expect:[1]
let%test_unit _ = [%test_result: int list] (to_list (of_list [1;2;3])) ~expect:[1;2;3]
let%test_unit _ = [%test_result: int list] (to_list (of_list [1;2;3] @ of_list [4;5;6])) ~expect:[1;2;3;4;5;6]
let%test_unit _ = [%test_result: int list] (to_list (concat (List.map ~f:of_list [[1;2];[3;4];[5;6]]))) ~expect:[1;2;3;4;5;6]
