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

val lines = String.tokens (fn c => c = #"\n");

val words = String.tokens Char.isSpace;

fun println s = (print s; print "\n");

fun expect s (NONE) = raise Fail s
  | expect s (SOME x) = x

(* [1518-11-01 00:00] Guard #10 begins shift
[1518-11-01 00:05] falls asleep
[1518-11-01 00:25] wakes up
[1518-11-01 00:30] falls asleep
[1518-11-01 00:55] wakes up
[1518-11-01 23:58] Guard #99 begins shift *)

datatype logEntryLine = GuardBegin of int
                      | FallAsleep of int
                      | WakeUp of int
fun toLogEntry s =
    case String.extract (s, 19, SOME 5) of
        "Guard" =>
        GuardBegin (expect "No parse" (Int.fromString (String.extract (s, 26, NONE))))
      | "falls" => FallAsleep (expect "No parse" (Int.fromString (String.extract (s, 15, NONE))))
      | "wakes" => WakeUp (expect "No parse" (Int.fromString (String.extract (s, 15, NONE))))
      | _ => raise Fail ("Could not parse: " ^ s)

fun combineEntries (GuardBegin id :: xs) =
    let fun aux acc id (FallAsleep fallAsleep :: WakeUp wakeUp :: rest) =
            let val () = println("Id: " ^ Int.toString id)
                val guardArr = Intmap.retrieve (acc, id)
                val minutes = List.tabulate (wakeUp - fallAsleep, fn i => fallAsleep + i)
                val () = println ("fall asleep : " ^ Int.toString fallAsleep ^
                                  ", wakeup: " ^ Int.toString wakeUp ^
                                  ", got minutes: " ^ String.concatWith ", " (map Int.toString minutes))
                val guardArr' =
                    List.foldl (fn (minute, acc) =>
                                   (Array.update (acc, minute,
                                                  Array.sub (acc, minute) + 1);
                                    acc))
                               guardArr minutes
            in aux (Intmap.insert (acc, id, guardArr')) id rest end
          | aux acc _ (GuardBegin id :: rest) =
            (case Intmap.peek (acc, id) of
                 SOME _ => aux acc id rest
               | NONE => aux (Intmap.insert (acc, id, Array.array(60, 0))) id rest)
          | aux acc _ [] =
            acc
          | aux _ _ entries = raise Fail "combineEntries.aux"
    in aux (Intmap.insert (Intmap.empty (), id, Array.array (60, 0))) id xs
    end
  | combineEntries _ = raise Fail "combineEntries"

fun maximumBy f [] = NONE
  | maximumBy f (x :: xs) =
    SOME (foldl (fn (x, acc) =>
                    case f (x, acc) of
                        GREATER => x
                      | _ => acc)
                x xs)

val enumerate : 'a list -> (int * 'a) list =
    let fun aux acc _ [] = rev acc
          | aux acc i (x :: xs)  = aux ((i, x) :: acc) (i+1) xs
    in aux [] 0 end

fun solve s =
    let val entries = map toLogEntry (Listsort.sort String.compare  (lines s))
        val combined = Intmap.listItems (combineEntries entries)
        val (SOME (id, schedule)) =
            maximumBy (fn (xs, ys) =>
                          Int.compare
                              (Array.foldl (fn (x, acc) => x + acc) 0 (#2 xs),
                               Array.foldl (fn (x, acc) => x + acc) 0 (#2 ys)))
                      combined
        val schedule' = enumerate (Array.foldr (op::) [] schedule)
        val () = println ("Schedule: " ^ String.concatWith ", " (map (fn (minute, x) =>
                                                                         Int.toString minute ^
                                                                         ":" ^ Int.toString x)
                                                                     schedule'))
        val () = println ("Guard most asleep : " ^ Int.toString id)
        val mostAsleepAt = #1 (expect "no max"
                                      (maximumBy (fn ((_, x), (_, y)) => Int.compare (x, y))
                                                 schedule'))
        val () = println ("Most asleep at: " ^
                          Int.toString mostAsleepAt)
        val () = println ("Product: " ^ Int.toString (mostAsleepAt * id))
    in () end

val () =
    let val input = TextIO.inputAll (TextIO.stdIn)
    in solve input
    end;
