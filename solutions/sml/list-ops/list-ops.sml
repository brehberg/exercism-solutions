(* fun append (list1: int list, list2: int list): int list = *)
fun append ([], list2) = list2 : int list
  | append (item :: list1, list2) = item :: append(list1, list2)

(* fun concat (lists: int list list): int list = *)
fun concat [] = [] : int list
  | concat (list :: lists) = append (list, concat lists)

(* fun reverse (list: int list): int list = *)
fun reverse [] = [] : int list
  | reverse (item :: list) = append (reverse(list), [item])

(* fun filter (function: int -> bool, list: int list): int list = *)
fun filter (_, []) = [] : int list
  | filter (pred, item :: list) = if pred item then item :: filter (pred, list) else filter (pred, list)

(* fun map (function: int -> int, list: int list): int list = *)
fun map (_, []) = [] : int list
  | map (func, item :: list) = (func item) :: map (func, list)

(* fun length (ns: int list): int = *)
fun length [] = 0 : int
  | length (_ :: list) = 1 + length list

(* fun foldl (function: int * int -> int, initial: int, list: int list): int = *)
fun foldl (_, final, []) = final : int
  | foldl (func, final, item :: list) = foldl (func, func (final, item), list)

(* fun foldr (function: int * int -> int, initial: int, list: int list): int = *)
fun foldr (_, final, []) = final : int
  | foldr (func, final, item :: list) = func (item, foldr (func, final, list))
