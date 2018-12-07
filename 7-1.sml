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

fun toDependency s =
    (String.sub (s, 5), String.sub (s, 36))

fun dedup xs =
    let fun aux acc [] = acc
          | aux acc (x :: xs)  =
            if List.exists (fn y => x = y) acc then
                aux acc xs
            else aux (x :: acc) xs
    in aux [] xs end

val sort = Listsort.sort Char.compare

fun resolveTargets acc [] [] = rev acc
  | resolveTargets acc deps [] = raise Fail ("Unresolved dependencies: " ^
                                             String.concatWith ", " (map (fn (c1, c2) => implode [c1, #":", c2]) deps))
  | resolveTargets acc deps (target :: targets) =
    let val (newTargets, newDeps) = List.partition (fn (x, y) => x = target) deps
        val newTargets' = List.filter (fn c => not (List.exists (fn (x, y) => y = c)
                                                                newDeps))
                                      (map #2 newTargets)
    in resolveTargets (target :: acc) newDeps (sort (newTargets' @ targets))
    end


fun solve s =
    let val deps = map toDependency (lines s)
        val allTargets = dedup (foldl (fn ((x1, x2), acc) => x1 :: x2 :: acc) [] deps)
        val initialTargets =
            sort (List.filter (fn target =>
                                  not (List.exists (fn (_, x) => x = target)
                                                   deps))
                              allTargets)

        val () = println (implode (resolveTargets [] deps initialTargets))
    in () end

val () =
    let val input = TextIO.inputAll (TextIO.stdIn)
    in solve input
    end;
