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

(* position=< 9,  1> velocity=< 0,  2> *)

fun parse s =
    let val toks = String.tokens Char.isSpace
                                 (String.map (fn c => if not (Char.isDigit c) andalso c <> #"-" then
                                                          #" "
                                                      else c) s)
    in case map Int.fromString toks of
           [SOME x, SOME y, SOME velX, SOME velY] => ((x,y), (velX, velY))
         | _ => raise Fail ("Couldn't parse : " ^ s)
    end

val step =
    map (fn ((x, y), (deltaX, deltaY)) =>
            ((x + deltaX, y + deltaY), (deltaX, deltaY)))


fun maximumBy f [] = raise Fail "maximumBy"
  | maximumBy f (x :: xs) =
    foldl (fn (x, acc) =>
              case f (x, acc) of
                  GREATER => x
                | _ => acc)
          x xs;

fun minimumBy f [] = raise Fail "maximumBy"
  | minimumBy f (x :: xs) =
    foldl (fn (x, acc) =>
              case f (x, acc) of
                  LESS => x
                | _ => acc)
          x xs;


fun printPoints xs =
    let val minX = #1 (#1 (minimumBy (fn (x, y) => Int.compare (#1 (#1 x), #1 (#1 y))) xs))
        val minY = #2 (#1 (minimumBy (fn (x, y) => Int.compare (#2 (#1 x), #2 (#1 y))) xs))
        val maxX = #1 (#1 (maximumBy (fn (x, y) => Int.compare (#1 (#1 x), #1 (#1 y))) xs))
        val maxY = #2 (#1 (maximumBy (fn (x, y) => Int.compare (#2 (#1 x), #2 (#1 y))) xs))
        val () = println ("minx: " ^ Int.toString minX ^
                          ", minY: " ^ Int.toString minY ^
                          ", maxX: " ^ Int.toString maxX ^
                          ", maxY: " ^ Int.toString maxY)
        fun aux x y =
            if y > maxY then
                ()
            else if x > maxX then
                (print "\n"; aux 0 (y + 1))
            else case List.find (fn ((x', y'), _) => x' = x andalso y' = y) xs of
                     SOME _ => (print "#"; aux (x + 1) y)
                   | NONE => (print "."; aux (x + 1) y)
    in aux minX minY end

fun alignedness xs =
    let val pointPoints =
            map (fn ((x, y), _) =>
                    foldl op+ 0
                          (List.map (fn ((x', y'), _) =>
                                        if abs (x - x') <= 1 andalso abs (y - y') <= 1
                                           andalso (x <> x' orelse y <> y') then
                                            ~ (abs (x - x')) - abs (y - y')
                                        else
                                            0)
                                    xs))
                xs
    in foldl (fn (0, _) => NONE
             | (score, NONE) => NONE
             | (score, SOME acc) => SOME (score + acc))
             (SOME 0)
             pointPoints
    end

fun solve s =
    let val points = map parse (lines s)
        fun aux i ps =
            let val () = if i mod 100 = 0 then
                         print "."
                     else ()
            in case alignedness ps of
                   SOME _ => (print "\n"; ps)
                 | NONE => aux (i + 1) (step ps)
            end
    in printPoints (aux 0 points) end

val () =
    let val input = TextIO.inputAll (TextIO.stdIn)
    in solve input
    end;
