load "Hashset";
load "Binarymap";
load "Int";
load "TextIO";
load "String";
load "Char";
load "Listsort";
load "Array2";
load "Substring";

val lines = String.tokens (fn c => c = #"\n");

val words = String.tokens Char.isSpace;

fun println s = (print s; print "\n");

fun parse s =
    let val withoutHash = Substring.triml 1 (Substring.full s)
        val (id, rest) = Substring.splitl (fn c => c <> #" ") withoutHash
        val (x, rest) = Substring.splitl (fn c => c <> #",") (Substring.triml 3 rest)
        val (y, rest) = Substring.splitl (fn c => c <> #":") (Substring.triml 1 rest)
        val (width, rest) = Substring.splitl (fn c => c <> #"x") (Substring.triml 2 rest)
        val height = Substring.triml 1 rest
    in case (Int.fromString (Substring.string id),
             Int.fromString (Substring.string x),
             Int.fromString (Substring.string y),
             Int.fromString (Substring.string width),
             Int.fromString (Substring.string height)) of
           (SOME id, SOME x, SOME y, SOME width, SOME height) =>
           { id = id, x = x, y = y, width = width, height = height }
         | _ => raise Fail ("Couldn't parse: " ^ s)
    end

val claimToFields =
    let fun aux acc { id, x, y, width, height } =
            if height = 0 then acc
            else aux (List.tabulate (width, fn i => (i + x, y)) @
                      acc)
                     { id = id, x = x, y = y+1, width = width, height = height-1 }
    in aux [] end

fun insertClaims arr claims =
    app (fn (x, y) => Array2.update (arr, x, y, Array2.sub (arr, x, y) + 1))
        (List.concat (map claimToFields claims))

fun solve input =
    let val claims = map parse (lines input)
        val arr = Array2.array (1000, 1000, 0)
        val () = insertClaims arr claims
    in Array2.fold Array2.ColMajor
                   (fn (n, acc) => if n > 1 then 1+acc else acc)
                   0
                   arr
    end

val () =
    let val input = TextIO.inputAll (TextIO.stdIn)
    in println (Int.toString (solve input))
    end;
