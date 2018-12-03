load "Hashset";
load "Binarymap";
load "Int";
load "TextIO";
load "String";
load "Char";
load "Listsort";

val lines = String.tokens (fn c => c = #"\n");

val words = String.tokens Char.isSpace;

fun println s = (print s; print "\n");

fun groupBy discriminator =
  let fun insert x [] = [(discriminator x, [x])]
        | insert x ((y, ys) :: rest) =
          if discriminator x = y then
              (y, x :: ys) :: rest
          else (y, ys) :: insert x rest
      fun aux acc [] = acc
        | aux acc (x :: xs) = aux (insert x acc) xs
  in aux [] end

fun hasTwoOfTheSame "" = false
  | hasTwoOfTheSame s =
    let val cs = explode s
        val grouped = groupBy (fn x => x) cs
    in List.exists (fn (_, xs) => length xs = 2) grouped end

fun hasThreeOfTheSame "" = false
  | hasThreeOfTheSame s =
    let val cs = explode s
        val grouped = groupBy (fn x => x) cs
    in List.exists (fn (_, xs) => length xs = 3) grouped end

fun solve input =
    let val words = lines input
    in (op* ) (length (List.filter hasTwoOfTheSame words),
               length (List.filter hasThreeOfTheSame words))
    end

val () =
    let val input = TextIO.inputAll (TextIO.stdIn)
    in println (Int.toString (solve input))
    end;
