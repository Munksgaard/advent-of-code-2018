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

fun sub (arr, i, j) =
    Array2.sub (arr, i, j)
    handle Subscript => #" "

fun parse s =
    let val arr = Array2.fromList (map explode (lines s))
        fun folder (i, j, x, acc) =
            case x of
                #">" => (i, j, #"-", 0) :: acc
              | #"<" => (i, j, #"-", 0) :: acc
              | #"v" => (i, j, #"|", 0) :: acc
              | #"^" => (i, j, #"|", 0) :: acc
              | _ => acc
        val carts = Array2.foldi Array2.ColMajor folder []
                                 { base = arr, col = 0, ncols = NONE,
                                   row = 0, nrows = NONE }

    in (arr, carts) end

fun direction (#">") (j, i) = (j, i + 1)
  | direction (#"v") (j, i) = (j + 1, i)
  | direction (#"<") (j, i) = (j, i - 1)
  | direction (#"^") (j, i) = (j - 1, i)
  | direction _ _ = raise Fail "direction"

fun turnLeft #">" = #"^"
  | turnLeft #"^" = #"<"
  | turnLeft #"<" = #"v"
  | turnLeft #"v" = #">"
  | turnLeft _ = raise Fail "turnLeft"

fun turnRight #">" = #"v"
  | turnRight #"v" = #"<"
  | turnRight #"<" = #"^"
  | turnRight #"^" = #">"
  | turnRight _ = raise Fail "rotateLeft"


fun toString arr =
    (Array2.appi Array2.RowMajor (fn (i, j, x) =>
                                     if j = 0 then print (implode [#"\n", x])
                                     else print (valOf (String.fromString (Char.toString x))))
                 { base = arr, row = 0, col = 0, nrows = NONE, ncols = NONE };
     println "")

fun turn (x, turnCount) =
    case turnCount mod 3 of
        0 => turnLeft x
      | 1 => x
      | _ => turnRight x

fun cmp ((i, j, _, _), (i', j', _, _)) =
    case Int.compare (i, i') of
        EQUAL => Int.compare (j, j')
      | x => x

fun step arr (i, j, underneath, turnCount) =
    let val x = Array2.sub (arr, i, j)
        val (i', j') = direction x (i, j)
        val () = Array2.update (arr, i, j, underneath)
    in case (x, Array2.sub (arr, i', j')) of
           (* Right moving cart *)
             (#">", new as #"-") => (Array2.update (arr, i', j', #">");
                                     (i', j', new, turnCount))
           | (#">", new as #"\\") => (Array2.update (arr, i', j', #"v");
                                      (i', j', new, turnCount))
           | (#">", new as #"/") => (Array2.update (arr, i', j', #"^");
                                     (i', j', new, turnCount))
           | (#">", new as #"+") => (Array2.update (arr, i', j', turn (x, turnCount));
                                     (i', j', new, turnCount + 1))
           (* Down moving cart *)
           | (#"v", new as #"|") => (Array2.update (arr, i', j', #"v");
                                     (i', j', new, turnCount))
           | (#"v", new as #"\\") => (Array2.update (arr, i', j', #">");
                                     (i', j', new, turnCount))
           | (#"v", new as #"/") => (Array2.update (arr, i', j', #"<");
                                     (i', j', new, turnCount))
           | (#"v", new as #"+") => (Array2.update (arr, i', j', turn (x, turnCount));
                                     (i', j', new, turnCount + 1))
           (* Left moving cart *)
           | (#"<", new as #"-") => (Array2.update (arr, i', j', #"<");
                                     (i', j', new, turnCount))
           | (#"<", new as #"\\") => (Array2.update (arr, i', j', #"^");
                                     (i', j', new, turnCount))
           | (#"<", new as #"/") => (Array2.update (arr, i', j', #"v");
                                     (i', j', new, turnCount))
           | (#"<", new as #"+") => (Array2.update (arr, i', j', turn (x, turnCount));
                                     (i', j', new, turnCount + 1))
           (* Up moving cart *)
           | (#"^", new as #"|") => (Array2.update (arr, i', j', #"^");
                                     (i', j', new, turnCount))
           | (#"^", new as #"\\") => (Array2.update (arr, i', j', #"<");
                                     (i', j', new, turnCount))
           | (#"^", new as #"/") => (Array2.update (arr, i', j', #">");
                                     (i', j', new, turnCount))
           | (#"^", new as #"+") => (Array2.update (arr, i', j', turn (x, turnCount));
                                     (i', j', new, turnCount + 1))
           (* Crash *)
           | (_, y) => (Array2.update (arr, i', j', #"X");
                        toString arr;
                        raise Fail ("Crash! " ^ Char.toString x ^
                              ", " ^ Int.toString j' ^ ", " ^ Int.toString i' ^
                              ", " ^ Char.toString y ))
    end

fun tick arr carts =
    Listsort.sort cmp (map (step arr) carts)

fun solve s =
    let val (arr, carts) = parse s
        fun loop carts = (loop (tick arr carts))
    in loop carts end

val () =
    let val input = TextIO.inputAll (TextIO.stdIn)
        val res = solve input
    in println (Int.toString res)
    end;
