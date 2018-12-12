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

fun enumerate xs =
    let fun aux acc _ [] = rev acc
          | aux acc i (x :: xs) =
            aux ((i, x) :: acc) (i+1) xs
    in aux [] 0 xs end

fun parse s =
    let val (x :: xs) = lines s
        fun toBool #"#" = true
          | toBool _ = false
        val init = enumerate (map toBool (explode (String.extract (x, 15, NONE))))
        val rules = List.mapPartial (fn ruleLine =>
                                        case explode ruleLine of
                                            [x1, x2, x3, x4, x5, _, _, _, _, #"#"] =>
                                            SOME (toBool x1, toBool x2, toBool x3, toBool x4, toBool x5)
                                          | _ =>
                                            NONE)
                                    xs
    in (init, rules) end

fun nextFive [] = raise Fail "nextFive"
  | nextFive [(i, x1)] = (i + 2, (x1, false, false, false, false))
  | nextFive [(_, x1), (i, x2)] = (i + 1, (x1, x2, false, false, false))
  | nextFive [(_, x1), (_, x2), (i, x3)] = (i, (x1, x2, x3, false, false))
  | nextFive [(_, x1), (_, x2), (i, x3), (_, x4)] = (i, (x1, x2, x3, x4, false))
  | nextFive ((_, x1) :: (_, x2) :: (i, x3) :: (_, x4) :: (_, x5) ::_ ) = (i, (x1, x2, x3, x4, x5))

fun pad [] = enumerate [false, false, false, false, false]
  | pad ((i, b) :: xs) = (i-4, false) :: (i-3, false) :: (i-2, false) :: (i-1, false) :: (i, b) :: xs


fun step rules xs =
    let fun aux acc [] = rev acc
          | aux acc (xs as (_ :: rest)) =
            let val (i, pattern) = nextFive xs
            in case List.find (fn rule => rule = pattern) rules of
                   SOME _ => aux ((i, true) :: acc) rest
                 | _ => aux ((i, false) :: acc) rest
            end
    in aux [] (pad xs) end

fun toString xs =
    let val s = implode (map (fn (_, true) => #"#" | _ => #".") xs)
        val first = #1 (hd xs)
        val numbers = implode (List.tabulate (String.size s, fn i => Char.chr (((i + first) mod 10) + 48)))
    in numbers ^ "\n" ^ s end

fun dropWhile p [] = []
  | dropWhile p (x :: xs) = if p x then dropWhile p xs else (x :: xs)

fun dropWhileBack (acc, acc') p [] = rev acc
  | dropWhileBack (acc, acc') p (x :: xs) =
    if p x then
        dropWhileBack (acc, x :: acc') p xs
    else
        dropWhileBack (x :: acc' @ acc, []) p xs

fun 'a prune xs : ('a * bool) list = dropWhileBack ([], []) (not o #2) (dropWhile (not o #2) xs)


fun steps 0 _ state = state
  | steps n rules state =
    let val next = prune (step rules state)
        (* val () = println (toString next) *)
    in if map #2 next = map #2 state then
           let val offset = #1 (hd next) - #1 (hd state)
           in map (fn (i, x) => (i + offset * n, x)) state end
           (* raise Fail ("Found reoccuring thingy at iteration -" ^ Int.toString n) *)
       else
           steps (n - 1) rules next
    end

fun solve s =
    let val (init, rules) = parse s
        val res = steps 50000000000 rules init
    in foldl (fn ((i, true), acc) => i + acc
               | (_, acc) => acc)
             0
             res
    end

val () =
    let val input = TextIO.inputAll (TextIO.stdIn)
        val res = solve input
    in println (Int.toString res)
    end;
