datatype 'a trie = 
  Root of 'a option * 'a trie list 
  | Node of 'a option * char * 'a trie list 

fun lookup_branch [] [] = NONE 
  | lookup_branch (node: int trie list) [x] = 
    let 
      val Node(value, key, _) = (hd node)
    in 
      if key = x then value else lookup_branch (tl node) [x]
    end
  | lookup_branch (node: int trie list) (x::xs)= 
    let 
      val Node(value, key, _) = (hd node)
    in 
      if key = x then value 
      else lookup_branch (tl node) xs
    end




