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

val timeSort = Listsort.sort (fn ((_, t1), (_, t2)) => Int.compare (t1, t2))

fun secondsToComplete c = 61 + ord c - ord #"A"

val splitAt =
    let fun aux acc _ [] = (rev acc, [])
          | aux acc 0 xs = (rev acc, xs)
          | aux acc n (x :: xs) =
            aux (x :: acc) (n - 1) xs
    in aux [] end

fun subtractTime n = map (fn (c, t) => (c, t - n))

fun resolveTargets _ timeSpent [] [] [] = timeSpent
  | resolveTargets idleWorkers timeSpent deps targets workerQueue =
    if idleWorkers = 0 orelse null targets then
        case workerQueue of
            (done, time) :: rest =>
            let val () = println ("worker " ^ Char.toString done ^
                                  " done after " ^ Int.toString time ^
                                 ", total: " ^ Int.toString (timeSpent + time))
                val (newTargets, newDeps) = List.partition (fn (x, y) => x = done) deps
                val newTargets' = List.filter (fn c => not (List.exists (fn (x, y) => y = c)
                                                                        newDeps))
                                              (map #2 newTargets)
            in resolveTargets (idleWorkers + 1)
                              (timeSpent + time)
                              newDeps
                              (sort (newTargets' @ targets))
                              (subtractTime time rest)
            end
          | _ => raise Fail ("No workers")
    else
        let val nextTarget = hd targets
        in resolveTargets (idleWorkers - 1)
                          timeSpent
                          deps
                          (tl targets)
                          (timeSort ((nextTarget, secondsToComplete nextTarget) :: workerQueue))
        end

fun solve s =
    let val deps = map toDependency (lines s)
        val allTargets = dedup (foldl (fn ((x1, x2), acc) => x1 :: x2 :: acc) [] deps)
        val initialTargets =
            sort (List.filter (fn target =>
                                  not (List.exists (fn (_, x) => x = target)
                                                   deps))
                              allTargets)

        val () = println (Int.toString (resolveTargets 5 0 deps initialTargets []))
    in () end

val () =
    let val input = TextIO.inputAll (TextIO.stdIn)
    in solve input
    end;
