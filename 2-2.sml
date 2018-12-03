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

fun hamming [] [] = 0
  | hamming [] (x :: xs) = 1 + hamming [] xs
  | hamming (x :: xs) [] = 1 + hamming [] xs
  | hamming (x :: xs) (y :: ys) =
    if x = y then hamming xs ys
    else 1 + hamming xs ys

fun solve input =
    let val words = map explode (lines input)
        fun aux [] = raise Fail "No match found"
          | aux (x :: xs) =
            case List.find (fn y => hamming x y = 1) xs of
                SOME y => println ("Found matches: \n" ^ implode x ^ "\n" ^ implode y)
              | NONE => aux xs
    in aux words
    end

val () =
    let val input = TextIO.inputAll (TextIO.stdIn)
    in solve input
    end;
