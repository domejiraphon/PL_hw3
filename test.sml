fun test1 (x, []) = [x]
  | test1(x, y::ys) = x :: test1(y, ys);

test1(2, [1,2]);
test1("hello", ["there"]);

fun test2 pred [] y = [y]
  | test2 pred (x::xs) y = pred (x, y) :: [y];

(*test2((fn (i, j) => i+1) 2);*)

(*Question 5.1
fun convertback [] = []
  | convertback (x::xs) = 
    let 
      fun intFromChar(x) = let val SOME i = Int.fromString(Char.toString(x)) in i end
    in
      intFromChar(x):: (convertback xs)
    end

fun digitsPos(x) = convertback(explode(Int.toString(x)));*)
fun digitsPos(x) = if x < 10 then [x] else digitsPos(x div 10) @ [x mod 10];

digitsPos(3124);
digitsPos(352663);

(*Question 5.2*)



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