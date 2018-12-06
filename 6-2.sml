load "Int";
load "Array2";
load "Substring";
load "Option";
load "Intmap";

fun println s = (print s; print "\n");

val lines = String.tokens (fn c => c = #"\n");

fun toCoordinates s =
    case List.mapPartial Int.fromString (String.fields (fn c => c = #",") s) of
        [x, y] => (x, y)
      | _ => raise Fail ("Couldn't parse " ^ s);

fun toString (x, y) = "(" ^ Int.toString x ^ ", " ^ Int.toString y ^ ")";

fun maximumBy f [] = raise Fail "maximumBy"
  | maximumBy f (x :: xs) =
    foldl (fn (x, acc) =>
              case f (x, acc) of
                  GREATER => x
                | _ => acc)
          x xs;

fun minimumBy f [] = raise Fail "maximumBy"
  | minimumBy f (x :: xs) =
    foldl (fn (_, []) => raise Fail "maximumBy.foldl"
            | (x, acc as (x' :: _)) =>
              case f (x, x') of
                  LESS => [x]
                | EQUAL => x :: acc
                | GREATER => acc)
          [x] xs;

fun manhattanDistance (x1, y1) (x2, y2) =
    Int.abs (x1 - x2) + Int.abs (y1 - y2);

datatype closest_coordinate = Tied
                    | ClosestTo of int * int

fun closestToString Tied = "Tied"
  | closestToString (ClosestTo p) = "ClosestTo " ^ toString p;

fun compare ((x1, y1), (x2, y2)) =
    case Int.compare (x1, x2) of
        EQUAL => Int.compare (y1, y2)
      | result => result;

fun getClosest points p =
    case minimumBy (fn (p1, p2) => Int.compare (manhattanDistance p p1,
                                                manhattanDistance p p2))
                   points of
        [x] => ClosestTo x
      | (_ :: _) => Tied
      | [] => raise Fail "getClosest";

fun dedup xs =
    let fun aux acc [] = acc
          | aux acc (x :: xs)  =
            if List.exists (fn y => x = y) acc then
                aux acc xs
            else aux (x :: acc) xs
    in aux [] xs end;

fun groupBy discriminator =
  let fun insert x [] = [(discriminator x, [x])]
        | insert x ((y, ys) :: rest) =
          if discriminator x = y then
              (y, x :: ys) :: rest
          else (y, ys) :: insert x rest
      fun aux acc [] = acc
        | aux acc (x :: xs) = aux (insert x acc) xs
  in aux [] end;

fun totalDistance points p =
    foldl op+ 0 (map (manhattanDistance p) points)

fun solve maxDistance s =
    let val coords = map toCoordinates (lines s)
        val maxX = #1 (maximumBy (fn ((x1, _), (x2, _)) => Int.compare (x1, x2)) coords)
        val maxY = #2 (maximumBy (fn ((_, y1), (_, y2)) => Int.compare (y1, y2)) coords)
        val () = println ("Max x: " ^ Int.toString maxX ^ ", max y: " ^ Int.toString maxY)
        val arr = Array2.tabulate Array2.ColMajor (maxX + 1, maxY + 1, totalDistance coords)
        val inRegion = Array2.foldi Array2.ColMajor
                                    (fn (x, y, res, acc) =>
                                        if res < maxDistance then
                                            1 + acc
                                        else
                                            acc)
                                    0
                                    { base = arr
                                    , col = 0
                                    , ncols = NONE
                                    , row = 0
                                    , nrows = NONE }
        val () = println ("in region: " ^ Int.toString inRegion)
    in () end;

val () =
    let val input = TextIO.inputAll (TextIO.stdIn)
    in solve 10000 input
    end;
