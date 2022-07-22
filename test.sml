fun test1 (x, []) = [x]
  | test1(x, y::ys) = x :: test1(y, ys);

test1(2, [1,2]);
test1("hello", ["there"]);

fun test2 pred [] y = y
  | test2 pred (x::xs) y = test2 pred xs (pred(x, y));

(*test2 (fn (x, y) => y) [1,2,5] 4;*)

datatype ('a, 'b) tree = Leaf of 'a
                        | Node of 'a * 'b;
fun test3 (Node(x, y)) pred = 
  let
    val tmp = pred x
  in
    Node(x, tmp)
  end


fun digitsPos(x) = if x < 10 then [x] else digitsPos(x div 10) @ [x mod 10];

digitsPos(3124);
digitsPos(352663);

(*Question 5.2*)

fun additivePersistence(x) = 
  let 
    fun sum_all_digits(x) = if x < 10 then x else x mod 10 + sum_all_digits(x div 10)
    fun count_digits_length(x) = if x < 10 then 1 else 1 + count_digits_length(x div 10)
    val tmp = sum_all_digits(x)
    val tmp_length = count_digits_length(x)
  in
    if tmp_length = 1 then 0 else 1+ additivePersistence(tmp)
  end

fun digitalRoot(x) = 
  let 
    fun sum_all_digits(x) = if x < 10 then x else x mod 10 + sum_all_digits(x div 10)
    fun count_digits_length(x) = if x < 10 then 1 else 1 + count_digits_length(x div 10)
    val tmp = sum_all_digits(x)
    val tmp_length = count_digits_length(x)
  in
    if tmp_length = 1 then tmp else digitalRoot(tmp)
  end

(*Question 5.3*)
fun alternate(L) = 
  if L = nil then 0 else hd(L) + sub(tl(L))
and sub(L) = 
  if L = nil then 0 else 0 - alternate(tl(L));

alternate [1,2,3,4];

(*Question 5.4*)
fun alternate2 [] f g = 0
  | alternate2 [x1, x2] f g = f(x1, x2)
  | alternate2 (x1::x2::xs) f g = 
    let 
      val tmp = f(x1, x2) :: xs
    in 
      alternate3 tmp f g
    end 
and alternate3 [] f g = 0
  | alternate3 [x1, x2] f g = g(x1, x2)
  | alternate3 (x1::x2::xs) f g = 
    let 
      val tmp = g(x1, x2) :: xs
    in 
      alternate2 tmp f g
    end 

(*Question 5.5*)
fun scan_left f y [] = []
  | scan_left f y (x::xs) = 
  let
    fun acc (y, ys) =
      if null ys 
        then []
      else
        (f (hd ys) y) :: acc((f (hd ys) y), tl ys)
  in 
    y :: acc(y, x::xs)
  end 

(*Question 5.6*)
fun zipRecycle(xs, ys) = 
  let
    fun helper(x, y, len) =
      if len = 0 
        then [] 
      else
        if null x 
          then helper(xs, y, len) 
        else 
          if null y
            then helper(x, ys, len) 
          else
            (hd x, hd y) :: helper(tl x, tl y, len - 1)
    fun list_length x = if null x then 0 else 1 + list_length (tl x)
    val len_y = list_length ys
  in
    helper(xs, ys, len_y)
  end
(*
zipRecycle([1,2,3], ["a","b","c"]);
zipRecycle([1,2,3,4,5], ["a", "b","c"]);
zipRecycle([1,2,3], ["a", "b", "c","d","e"]);*)
(*Question 5.7*)
fun add x y = x + y;
fun bind NONE (SOME y) f = NONE 
  | bind (SOME x) NONE f = NONE
  | bind (SOME x) (SOME y) f = SOME (f x y)


(*Question 5.8*)
fun lookup (xs: (string * int) list) (key: string) =
  if null xs 
    then NONE 
  else
    if #1(hd xs) = key 
      then SOME (#2(hd xs)) 
    else
      lookup (tl xs) key

(*Question 5.9*)
fun getitem key xs = 
  let
    fun list_length x = if null x then 0 else 1 + list_length (tl x)
    val len_x = list_length xs
  in 
    if key > len_x
      then NONE 
    else
      if key = 1
        then SOME (hd xs)
      else 
        getitem (key-1) (tl xs)
  end 

fun defaultToThree (NONE : int option):int = 3
  | defaultToThree (SOME x) = x

(*Question 5.10*)
fun getitem2 NONE xs = NONE
  | getitem2 (SOME key) [] = NONE
  | getitem2 (SOME key) xs = 
    getitem key xs; 


