load "Hashset";
load "Binarymap";
load "Int";
load "TextIO";
load "String";

val lines = String.tokens (fn c => c = #"\n");

val words = String.tokens Char.isSpace;

fun println s = (print s; print "\n");

fun solve input =
    let val words = List.concat (List.map (String.fields (fn c => c = #",")) (lines input))
        val changes = List.map (fn s => case Int.fromString s of
                                            SOME r => r
                                          | NONE => raise Fail ("couldn't parse " ^ s))
                               words
    in foldl (op+) 0 changes end

val () =
    let val input = TextIO.inputAll (TextIO.stdIn)
    in println (Int.toString (solve input))
    end;
