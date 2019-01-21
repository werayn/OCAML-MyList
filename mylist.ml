type 'a my_list =
| Item of ('a * 'a my_list)
| Empty;;

let cons l1 l2 = Item(l1, l2);;

let length my_list =
  let rec length_aux acc = function
    | Empty -> acc
    | Item (head, tail) -> length_aux (acc + 1) tail
  in length_aux 0 my_list;;

let hd = function
  | Empty -> raise (Failure "hd")
  | Item (head, tail) -> head;;

let tl = function
  | Empty -> raise (Failure "tl")
  | Item (head, tail) -> tail;;

let nth my_list arg =
if 0 > arg
then
  raise (Invalid_argument "List.nth")
else
  let rec nth_aux acc = function
    | Empty -> raise (Failure "nth")
    | Item (head, tail) ->
      if acc = 0
      then head
      else nth_aux (acc - 1) tail
  in nth_aux arg my_list;;

let rev my_list =
  let rec rev_aux new_list = function
    | Empty -> new_list
    | Item (head, tail) -> rev_aux (Item(head, new_list)) tail
in rev_aux Empty my_list;;

let append list1 list2 =
  let rec rev_aux list2 = function
    | Empty -> list2
    | Item (head, tail) -> rev_aux (Item(head, list2)) tail
  in rev_aux list2 (rev list1);;

let rev_append list1 list2 = append (rev list1) list2;;

let flatten my_list =
  let rec flatten_aux new_list = function
    | Empty -> new_list
    | Item (head, tail) -> flatten_aux (append new_list head) tail
  in flatten_aux Empty my_list;;

let rec iter my_func = function
  | Empty -> ()
  | Item (head, tail) -> my_func head; iter my_func tail;;

let map my_func my_list =
  let rec map_aux new_list my_func = function
    | Empty -> (rev new_list)
    | Item (head, tail) -> map_aux (Item (my_func head, new_list)) my_func tail
  in map_aux Empty my_func my_list;;

let rec fold_left my_func new_list = function
  | Empty -> new_list
  | Item (head, tail) -> fold_left my_func (my_func new_list head) tail;;

let rec for_all func = function
  | Empty -> true
  | Item (head, tail) ->
    if (func head)
    then for_all func tail
    else false;;

let rec exists func = function
  | Empty -> false
  | Item (head, tail) ->
    if (func head)
    then true
    else exists func tail;;

let rec mem arg = function
  | Empty -> false
  | Item (head, tail) ->
    if head = arg
    then true
    else mem arg tail;;

let rec memq arg = function
  | Empty -> false
  | Item (head, tail) ->
    if head == arg
    then true
    else mem arg tail;;

let rec find s l = match l with
	| Empty -> raise (Failure "Not_found")
	| Item(value, next) -> if s value = true then l else find s next

let filter my_func my_list =
  let rec filter_aux new_list my_func = function
    | Empty -> (rev new_list)
    | Item (head, tail) ->
      if (my_func head)
      then filter_aux (Item (head, new_list)) my_func tail
      else filter_aux new_list my_func tail
  in filter_aux Empty my_func my_list;;

let split my_list =
  let rec split_aux (my_key, my_value) = function
    | Empty -> (my_key, my_value)
    | Item ((key, value), tail) ->
      split_aux (Item (key, my_key), Item (value, my_value)) tail
  in split_aux (Empty, Empty) (rev my_list);;

let rec combine l1 l2 =
   match (l1, l2) with
   (Empty, Empty) -> Empty
      | (Item(a1, l1), Item(a2, l2)) -> Item((a1, a2), combine l1 l2)
      | (_,_) -> raise (Invalid_argument "MyList.combine");;

let mem_assoc my_key my_list =
  let rec mem_assoc_aux my_key = function
    | Empty -> false
    | Item ((key, value), tail) ->
      if key = my_key
      then true
      else mem_assoc_aux my_key tail
  in mem_assoc_aux my_key my_list;;

let assoc my_key my_list =
  let rec assoc_aux my_key = function
    | Empty -> raise Not_found
    | Item ((key, value), tail) ->
      if key = my_key
      then value
      else assoc_aux my_key tail
  in assoc_aux my_key my_list;;
