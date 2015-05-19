open Core.Std
let tree_fold ~f ~default =
  let rec pairwise_f accum = function
    | [] -> List.rev accum
    | [x] -> List.rev (x :: accum)
    | x :: y :: zs -> pairwise_f (f x y :: accum) zs
  in
  let rec
    go = function
    | [] -> default
    | [x] -> x
    | l -> go (pairwise_f [] l)
  in
  go

let test expect list =
  <:test_result<string>> ~expect
    (tree_fold ~f:(fun a b -> "(" ^ a  ^ "+" ^ b ^ ")") ~default:"<empty>" list)

TEST_UNIT "length 0" =
  test "<empty>" []

TEST_UNIT "length 1" =
  test "a" ["a"]

TEST_UNIT "length 2" =
  test "(a+b)" ["a"; "b"]

TEST_UNIT "length 3" =
  test "((a+b)+c)" ["a"; "b"; "c"]

TEST_UNIT "length 4" =
  test "((a+b)+(c+d))" ["a"; "b"; "c"; "d"]

TEST_UNIT "length 5" =
  test "(((a+b)+(c+d))+e)" ["a";"b";"c";"d";"e"]
