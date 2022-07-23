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

  fun lookup_branch [] [] = NONE 
    | lookup_branch (node: 'a trie list) [x] = 
      let 
        val Node(value, key, _) = (hd node)
      in 
        if key = x then value else lookup_branch (tl node) [x]
      end
    | lookup_branch (node: 'a trie list) (x::xs)= 
      let 
        val Node(value, key, node_list) = (hd node)
      in 
        if key = x then lookup_depth((hd node), xs)
        else lookup_branch (tl node) xs
      end
  and lookup_depth((Root(value, [])), []) = value
      | lookup_depth((Root(value, [])), (x::xs)) = NONE
      |  lookup_depth((Node(value, key, [])), [x]) = 
        if x = key then value else NONE
      | lookup_depth((Node(value, key, [])), (x::xs)) = NONE 
      | lookup_depth((Node(value, key, (node::node_list))), [x]) = 
        if x = key then value else NONE 
      | lookup_depth((Node(value, key, (node::node_list))), (x::xs)) = 
        if x = key then lookup_depth(node, xs)
        else lookup_branch node_list xs 
      | lookup_depth((Root(value, (node::node_list))), (x::xs)) =
        lookup_branch (node::node_list) (x::xs)

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
    | child((node::trie_list), ((x::xs), value)) = 
        let
          val Node(node_value, node_key, node_list) = node 
        in 
          if x = node_key 
            then insert_helper(node, (xs, value)):: trie_list 
          else 
            node::child(trie_list, ((x::xs), value))
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
