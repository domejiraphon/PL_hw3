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
    | lookup_branch (node::node_list) [x] = 
      if #2 node = x then #1 node 
      else NONE 
    | lookup_branch (node::node_list) (x::xs)= 
      if #2 node = x then lookup_depth node xs
      else lookup_branch node_list xs 

  fun lookup_depth Root(value, []) [] = value
    | lookup_depth Node(value, key, []) [x] = 
      if x = key then value else NONE 
    | lookup_depth Node(value, key, []) (x::xs) = NONE 
    | lookup_depth Node(value, key, (node::node_list)) [x] = 
      if x = key then value else NONE 
    | lookup_depth Node(value, key, (node::node_list)) (x::xs) = 
      if x = key then lookup_depth node xs 
      else lookup_branch node_list xs 
    | lookup_depth Root(value, (node::node_list)) (x::xs) = 
      if #2 node = x then lookup_depth node xs 
      else lookup_branch node_list (x::xs)

  fun lookup trie key = 
    let 
      val char_list = explode(key)
    in 
      lookup_depth trie char_list
    end 
  
  

  

end;