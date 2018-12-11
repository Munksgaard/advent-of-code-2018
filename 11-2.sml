(* There is an off-by-2 error in the result. Add 2 to both the x and y
 * coordinate in order to get the right result. *)

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

fun powerlevel' serial (x, y) = powerlevel serial (x+1, y+1)

fun highest sumgrid (max, maxPoint, maxSize) size (i, j) =
    if size > 300 then
        (max, maxPoint, maxSize)
    else
        let val res = Array2.sub (sumgrid, i + size, j + size) - (* D *)
                      Array2.sub (sumgrid, i + size, j) -        (* B *)
                      Array2.sub (sumgrid, i, j + size) +        (* C *)
                      Array2.sub (sumgrid, i, j)                 (* A *)
                      handle Subscript => 0
        in if res > max then
               highest sumgrid (res, (i, j), size) (size + 1) (i, j)
           else
               highest sumgrid (max, maxPoint, maxSize) (size + 1) (i, j)
        end

fun solve serial =
    let val grid = Array2.tabulate Array2.ColMajor
                               (300, 300, powerlevel serial)
        val () = Array2.modifyi
                     Array2.ColMajor
                     (fn (i, j, x) =>
                         let val left = Array2.sub (grid, i - 1, j) handle Subscript => 0
                             val up = Array2.sub (grid, i, j - 1) handle Subscript => 0
                             val leftup = Array2.sub (grid, i - 1, j - 1) handle Subscript => 0
                         in left - leftup + up + x end)
                     { base = grid, row = 0, col = 0, nrows = NONE, ncols = NONE }
    in Array2.foldi Array2.ColMajor (fn (i, j, x, acc) =>
                                         highest grid acc 0 (i, j))
                     (0, (0, 0), 0)
                     { base = grid, row = 0, col = 0, nrows = NONE, ncols = NONE }
    end

val () =
    let val input = valOf (Int.fromString (TextIO.inputAll (TextIO.stdIn)))
        val (_, (x, y), size) = solve input
    in println (Int.toString x ^ "," ^ Int.toString y ^ "," ^ Int.toString size)
    end;
