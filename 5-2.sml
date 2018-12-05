load "Hashset";
load "Binarymap";
load "Int";
load "TextIO";
load "String";
load "Char";
load "Listsort";
load "Array2";
load "Substring";
load "Option";
load "Intmap";

fun println s = (print s; print "\n")

fun fix f x =
    let fun aux acc f x =
            let val x' = f x
            in if acc = x' then acc
               else aux x' f x' end
    in aux x f x end

fun oppositePolarity c1 c2 =
    (Char.isLower c1 andalso Char.isUpper c2 andalso Char.toUpper c1 = c2)
    orelse
    (Char.isUpper c1 andalso Char.isLower c2 andalso Char.toLower c1 = c2)

val react =
    rev o
    foldl (fn (c1, (c2 :: rest)) =>
              if oppositePolarity c1 c2 then
                  rest
              else c1 :: c2 :: rest
            | (c1, []) => [c1])
          []

val allTypes = explode "abcdefghijklmnopqrstuvwxyz"

fun minimumBy f [] = raise Fail "minimumBy"
  | minimumBy f (x :: xs) =
    foldl (fn (x, acc) =>
              case f (x, acc) of
                  LESS => x
                | _ => acc)
          x xs


fun solve s =
    let val cs = List.filter Char.isAlpha (explode s)
        val xs = map (fn type_ => List.filter (fn c => Char.toLower c <> type_) cs)
                     allTypes
        val xs' = map (length o fix react) xs
        val () = println (String.concatWith ", " (map Int.toString xs'))
        val shortest = minimumBy Int.compare xs'
        val () = println ("Shortest length: " ^ Int.toString shortest)
    in () end

val () =
    let val input = TextIO.inputAll (TextIO.stdIn)
    in solve input
    end;
