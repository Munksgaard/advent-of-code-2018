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

datatype mapSpace = Sand
                  | Clay
                  | RunningWater
                  | RestingWater

fun fromChar #"." = Sand
  | fromChar #"#" = Clay
  | fromChar #"|" = RunningWater
  | fromChar #"~" = RestingWater
  | fromChar c = raise Fail ("Couldn't parse " ^ implode [c])

fun toChar Sand = #"."
  | toChar Clay = #"#"
  | toChar RunningWater = #"|"
  | toChar RestingWater = #"~"

fun full arr =
    { base = arr, col = 0, row = 0, ncols = NONE, nrows = NONE }

fun printMap arr =
    let val width = Array2.nCols arr
        val minX = Array2.foldi Array2.RowMajor (fn (i, j, x, acc) => if x <> Sand then
                                                                          Int.min(j, acc)
                                                                      else acc)
                                500 (full arr)
        val () = println ("minX: " ^ Int.toString minX ^ ", width: " ^ Int.toString width)
        fun aux n = if n > width then ()
                    else (if n = 500 then print "+"
                          else print ".";
                          aux (n + 1))
        val () = aux minX
        val acc = Array2.appi
                      Array2.RowMajor
                      (fn (i, j, x) =>
                          (if j = 0 then
                               print "\n"
                           else ();
                           if j >= minX then
                               print (implode [toChar x])
                           else
                               ()))
                      (full arr)
        val () = println ""
    in () end

fun mapBoth f (x, y) = (f x, f y)

fun parse s =
    let val (maxX, maxY) =
            (mapBoth (foldl Int.max 0 o
                      List.concat o
                      map (map (valOf o Int.fromString) o
                           String.tokens (not o Char.isDigit))) o
             List.partition
                 (fn s => String.isSubstring "x" s) o
             String.tokens (not o
                            (fn c => Char.isDigit c orelse
                                     c = #"x" orelse c = #"="
                                     orelse c = #".")))
                s
        val arr = Array2.array (maxY + 1, maxX + 5, Sand)
        fun applyLine line =
            let val first = valOf (Int.fromString (String.extract (line, 2, NONE)))
                val ss = Substring.dropl (fn c => c <> #" ")
                                         (Substring.full line)
                val second = valOf (Int.fromString
                                        (Substring.string
                                             (Substring.triml 3 ss)))
                val third = valOf (Int.fromString
                                       (Substring.string
                                            (Substring.triml 2
                                                 (Substring.dropl
                                                      (fn c => c <> #".")
                                                      ss))))
                fun verticalLine i = if i > third then ()
                                     else (Array2.update (arr, i, first, Clay);
                                           verticalLine (i + 1))
                fun horizontalLine i = if i > third then ()
                                       else (Array2.update (arr, first, i, Clay);
                                             horizontalLine (i + 1))

            in if String.sub (line, 0) = #"x" then
                   verticalLine second
               else
                   horizontalLine second
            end

        val () = app applyLine (lines s)

    in arr end

fun isWater x = x = RunningWater orelse x = RestingWater

fun up (i, j) = (i - 1, j)
fun down (i, j) = (i + 1, j)
fun left (i, j) = (i, j - 1)
fun right (i, j) = (i, j + 1)

fun sub arr (i, j) = Array2.sub (arr, i, j)
fun update arr (i, j) x = Array2.update (arr, i, j, x)

fun canRun x = x = Sand orelse x = RunningWater

fun firstNot x arr pos direction =
    let val x' = sub arr pos
    in if x' = x then
           firstNot x arr (direction pos) direction
       else
           x'
    end

fun convertFrom from to arr pos direction =
    let val x' = sub arr pos
    in if x' = from then
           (update arr pos to;
            convertFrom from to arr (direction pos) direction)
       else ()
    end

fun posString (i, j) = "(" ^ Int.toString i ^ ", " ^ Int.toString j ^ ")"

fun followStream arr pos direction =
    let val x = sub arr (direction pos)
    in if x = RunningWater then
           followStream arr (direction pos) direction
       else
           pos
    end

fun runWater arr pos =
    if sub arr pos = RestingWater then ()
    else
    let val () = update arr pos RunningWater
        val beneath = sub arr (down pos)
        val toLeft = sub arr (left pos)
        val toRight = sub arr (right pos)
    in case (canRun beneath, toLeft, toRight) of
           (true, _, _) => runWater arr (down pos)
         | (false, Clay, Clay) => update arr pos RestingWater
         | (false, Sand, Sand) => (runWater arr (left pos);
                                   runWater arr (right pos))
         | (false, Sand, _) => runWater arr (left pos)
         | (false, _, Sand) => runWater arr (right pos)
         | (false, Clay, _) => if firstNot RunningWater arr pos right = Clay then
                                   convertFrom RunningWater RestingWater arr pos right
                               else
                                   runWater arr (followStream arr pos right)
         | (false, _, Clay) => if firstNot RunningWater arr pos left = Clay then
                                   convertFrom RunningWater RestingWater arr pos left
                               else
                                   runWater arr (followStream arr pos left)
         | (false, RestingWater, _) =>
           raise Fail ("RestingWater left of " ^ Int.toString (#1 pos) ^
                       ", " ^ Int.toString (#2 pos))
         | (false, _, RestingWater) =>
           raise Fail ("RestingWater right of " ^ Int.toString (#1 pos) ^
                       ", " ^ Int.toString (#2 pos))
         | (false, RunningWater, RunningWater) =>
           let val leftStreamEnd = followStream arr pos left
               val rightStreamEnd = followStream arr pos right
           in (if sub arr (left leftStreamEnd) = Sand then
                   runWater arr leftStreamEnd
               else ();
               if sub arr (right rightStreamEnd) = Sand then
                   runWater arr rightStreamEnd
               else ())
           end
    end
    handle Subscript => ()

fun copy arr =
    let val (m, n) = Array2.dimensions arr
    in Array2.tabulate Array2.RowMajor (m, n, fn pos => sub arr pos) end

fun diff arr arr' =
    (Array2.appi Array2.RowMajor
                 (fn (i, j, x) => if Array2.sub (arr', i, j) <> x then
                                      raise Fail "diff"
                                  else ())
                 (full arr);
     false)
    handle _ => true

fun runWater' n arr =
    let val arr' = copy arr
        val () = runWater arr (0, 500) handle _ => (println "Error!\n\n";
                                                    printMap arr';
                                                    printMap arr;
                                                    raise Fail "bla!")
    in if diff arr arr' then
           runWater' (n + 1) arr
       else
           ()
    end

fun countWater arr =
    let val (minY, maxY) = Array2.foldi
                               Array2.RowMajor
                               (fn (i, j, x, (minY, maxY)) =>
                                   (if x = Clay then
                                        Int.min (i, minY)
                                    else
                                        minY ,
                                    if x = Clay then
                                        Int.max (i, maxY)
                                    else
                                        maxY))
                               (#1 (Array2.dimensions arr), 0)
                               (full arr)
    in Array2.foldi
           Array2.RowMajor
           (fn (_, _, x, acc) => if isWater x then 1 + acc else acc)
           0
           { base = arr
           , row = minY
           , nrows = SOME (maxY - minY + 1)
           , col = 0
           , ncols = NONE }
    end

val () =
    let val input = TextIO.inputAll (TextIO.stdIn)
        val arr = parse input
        val () = runWater' 0 arr
    in println (Int.toString (countWater arr))
    end
