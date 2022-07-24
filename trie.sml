signature DICT = 
sig 
  type key = string
  type 'a entry = key * 'a
  type 'a dict 

  val empty: 'a dict 
  val lookup : 'a dict -> key -> 'a option 
  val insert: 'a dict * 'a entry -> 'a dict 
end;

structure Trie :> DICT = 
struct 
  type key = string 
  type 'a entry = key * 'a 

  datatype 'a trie = 
    Root of 'a option * 'a trie list 
    | Node of 'a option * char * 'a trie list 
  
  type 'a dict = 'a trie 

  val empty = Root(NONE, [])

  fun lookup_branch [] xs = NONE 
    | lookup_branch (node: 'a trie list) [x]= 
      let 
        val Node(value, key, node_list) = (hd node)
      in 
        if key = x then value
        else NONE
      end
    | lookup_branch (node: 'a trie list) (x::xs)= 
      let 
        val Node(value, key, node_list) = (hd node)
      in 
        if key = x then lookup_depth((hd node), xs)
        else lookup_branch (tl node) xs
      end
  and lookup_depth((Root(value, trie_list)), []) = value
      | lookup_depth((Root(value, trie_list)), (x::xs)) = lookup_branch trie_list (x::xs)
      | lookup_depth((Node(value, key, trie_list)), []) = value
      | lookup_depth((Node(value, key, trie_list)), (x::xs)) = lookup_branch trie_list (x::xs)
     

  fun lookup trie key = 
    let 
      val char_list = explode(key)
    in 
      lookup_depth(trie, char_list)
    end 

  fun child([], ([x], value)) = 
      [Node(SOME value, x, [])]
    | child([], ((x::xs), value)) = 
        [Node(NONE, x, child([], (xs, value)))]
    | child(node: 'a trie list, ((x::xs), value)) = 
        let
          val Node(node_value, node_key, node_list) = (hd node) 
        in 
          if x = node_key 
            then insert_helper((hd node), (xs, value)):: (tl node)
          else 
            (hd node)::child((tl node), ((x::xs), value))
        end
  and insert_helper(Root(trie_value, trie_list), (nil, value)) = 
        Root(SOME value, trie_list)
    | insert_helper(Root(trie_value, trie_list), (key, value)) = 
        Root(trie_value, child(trie_list, (key, value)))
    | insert_helper(Node(node_value, node_key, trie_list), (nil, value)) = 
        Node(SOME value, node_key, trie_list)
    | insert_helper(Node(node_value, node_key, trie_list), (key, value)) = 
        Node(node_value, node_key, child(trie_list, (key, value)))

  fun insert (trie, (key, value)) = 
    let
      val char_list = explode key 
    in
      insert_helper(trie, (char_list, value))
    end
end;

open Trie; 

val trie = insert(empty, ("bad", 1));
val trie = insert(trie, ("badge", 2));
val trie = insert(trie, ("icon", 3));
val trie = insert(trie, ("", 4));
val trie = insert(trie, ("badly", 5));
(if trie = test_obj then print("insert passes\n") 
else print("insert fails\n"));
(*
val test1 = lookup trie "b";
if test1 = NONE then "Pass" else "Fail";

val test2 = lookup trie "bad";
if test2 = SOME 1 then "Pass" else "Fail";

val test2 = lookup trie "badge";
if test2 = SOME 2 then "Pass" else "Fail";

val test3 = lookup trie "badly";
if test2 = SOME 5 then "Pass" else "Fail";

val test4 = lookup trie "icon";
if test4 = SOME 3 then "Pass" else "Fail";

val test5 = lookup trie "";
if test5 = SOME 4 then "Pass" else "Fail";*)