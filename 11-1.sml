load "Option";
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

val lines = String.tokens (fn c => c = #"\n")

fun hundreds x = x div 100 mod 10

fun powerlevel serial (x, y) =
    if x > 300 orelse y > 300 then 0
    else let val rackId = x + 10
         in hundreds ((rackId * y + serial) * rackId) - 5 end

fun squarepower serial (x, y) =
    powerlevel serial (x, y) +
    powerlevel serial (x+1, y) +
    powerlevel serial (x+2, y) +
    powerlevel serial (x, y+1) +
    powerlevel serial (x+1, y+1) +
    powerlevel serial (x+2, y+1) +
    powerlevel serial (x, y+2) +
    powerlevel serial (x+1, y+2) +
    powerlevel serial (x+2, y+2)


fun highest (max, maxCoords) serial i j =
    if i > 300 then
        highest (max, maxCoords) serial 0 (j + 1)
    else if j > 300 then
        (max, maxCoords)
    else
        let val power = squarepower serial (i, j)
            (* val () = println ("computing for " ^ Int.toString i ^ *)
            (*                   ", " ^ Int.toString j) *)
        in if power > max then
               highest (power, (i, j)) serial (i + 1) j
           else
               highest (max, maxCoords) serial (i + 1) j
        end

fun solve serial =
    highest (0, (0, 0)) serial 1 1

val () =
    let val input = valOf (Int.fromString (TextIO.inputAll (TextIO.stdIn)))
        val (_, (x, y)) = solve input
    in println (Int.toString x ^ "," ^ Int.toString y)
    end;
