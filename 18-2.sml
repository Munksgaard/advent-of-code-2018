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

datatype mapSpace = Ground
                  | Trees
                  | Lumberyard

fun fromChar #"." = Ground
  | fromChar #"|" = Trees
  | fromChar #"#" = Lumberyard
  | fromChar c = raise Fail ("Couldn't parse " ^ implode [c])

fun toChar Ground = #"."
  | toChar Trees = #"|"
  | toChar Lumberyard = #"#"

fun full arr =
    { base = arr, col = 0, row = 0, ncols = NONE, nrows = NONE }

fun parse s =
    Array2.fromList (map (map fromChar o explode) (lines s))


fun printMap arr =
    let val () = Array2.appi
                     Array2.RowMajor
                     (fn (i, j, x) =>
                         (if j = 0 then
                              (print "\n";
                               print (implode [toChar x]))
                          else
                              print (implode [toChar x])))
                      (full arr)
        val () = println ""
    in () end

fun adjacent (i, j) =
    [(i + 1, j), (i + 1, j + 1), (i, j + 1), (i - 1, j + 1),
     (i - 1, j), (i - 1, j - 1), (i, j - 1), (i + 1, j - 1)]

fun step arr =
    let val (m, n) = Array2.dimensions arr

        fun threeAdjacent x (i, j) =
            length (List.filter (fn (i', j') => Array2.sub (arr, i', j') = x
                                                handle _ => false)
                                (adjacent (i, j))) >= 3

        fun stayLumberyard (i, j) =
            let val adj = adjacent (i, j)
            in List.exists (fn (i', j') => Array2.sub (arr, i', j') = Lumberyard
                                           handle _ => false)
                           adj
               andalso
               List.exists (fn (i', j') => Array2.sub (arr, i', j') = Trees
                                           handle _ => false)
                           adj
            end

        fun aux (i, j) =
            case Array2.sub (arr, i, j) of
                Ground => if threeAdjacent Trees (i, j) then
                              Trees
                          else
                              Ground
              | Trees => if threeAdjacent Lumberyard (i, j) then
                             Lumberyard
                         else
                             Trees
              | Lumberyard => if stayLumberyard (i, j) then
                                  Lumberyard
                              else
                                  Ground
        val arr' = Array2.tabulate Array2.RowMajor (m, n, aux)

    in arr' end

val score =
    op* o
    Array2.fold Array2.RowMajor (fn (Trees, (wooded, yards)) => (wooded + 1, yards)
                                  | (Lumberyard, (wooded, yards)) => (wooded, yards + 1)
                                  | (_, acc) => acc)
                (0, 0)


fun diff arr arr' =
    (Array2.appi Array2.RowMajor (fn (i, j, x) => if Array2.sub (arr', i, j) <> x then
                                                      raise Fail "diff"
                                                  else ())
                 (full arr);
     false)
    handle _ => true

fun stepTimes arr 0 = arr
  | stepTimes arr n = stepTimes (step arr) (n - 1)

fun solve 0 (old, oldN) arr = score arr
  | solve n (old, oldN) arr =
    let val arr' = step arr
    in if diff old arr' then
           solve (n - 1)
                 (if n mod 100 = 0 then (arr', n) else (old, oldN))
                 arr'
       else
           let val delta = oldN - n
           in score (stepTimes arr (n mod delta))
           end
    end

val () =
    let val input = TextIO.inputAll (TextIO.stdIn)
        val arr = parse input
    in println (Int.toString (solve 1000000000 (arr, 0) arr))
    end;
