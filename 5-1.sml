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

fun solve s =
    let val result = fix react (List.filter Char.isAlpha (explode s))
        val () = println (implode result)
        val () = println ("Length: " ^ Int.toString (length result))
    in () end

val () =
    let val input = TextIO.inputAll (TextIO.stdIn)
    in solve input
    end;
