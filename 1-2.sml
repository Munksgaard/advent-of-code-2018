load "Hashset";
load "Binarymap";
load "Int";
load "TextIO";
load "String";

val lines = String.tokens (fn c => c = #"\n");

val words = String.tokens Char.isSpace;

fun println s = (print s; print "\n");

fun findRepeating [] = raise Fail "findRepeating got empty input"
  | findRepeating (head :: changes) =
    let fun aux acc [] = aux acc (head :: changes)
          | aux (current, prev) (x :: xs) =
            let val new = current + x
                (* val () = println ("current: " ^ Int.toString current ^ *)
                (*                   ", x: " ^ Int.toString x ^ *)
                (*                   ", new: " ^ Int.toString new) *)
            in if List.exists (fn x => x = new) prev then
                   new
               else
                   aux (new, current :: prev) xs
            end
    in aux (head, []) changes
    end

fun solve input =
    let val words = List.concat (List.map (String.fields (fn c => c = #",")) (lines input))
        val changes = List.map (fn s => case Int.fromString s of
                                            SOME r => r
                                          | NONE => raise Fail ("couldn't parse " ^ s))
                               words
    in findRepeating changes end;

val () =
    let val input = TextIO.inputAll (TextIO.stdIn)
    in println (Int.toString (solve input))
    end;
