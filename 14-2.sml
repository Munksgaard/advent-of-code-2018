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

fun digits 0 = [0]
  | digits n =
    let fun aux acc 0 = acc
          | aux acc i = aux (i mod 10 :: acc) (i div 10)
    in aux [] n end

val digitString =
    String.concat o map Int.toString

fun nextTen arr n =
    List.tabulate (10, fn i => Array.sub (arr, n + i))

fun printState arr num elf1 elf2 =
    let fun aux i =
            if i >= num then print "\n"
            else let val x = Array.sub (arr, i)
                 in if i = elf1 then
                        (print ("(" ^ Int.toString x ^ ")"); aux (i + 1))
                    else if i = elf2 then
                        (print ("[" ^ Int.toString x ^ "]"); aux (i + 1))
                    else
                        (print (" " ^ Int.toString x ^ " "); aux (i + 1))
                 end
    in aux 0 end

fun isItThere arr lookingFor at =
    List.all (fn (i, d) => d = Array.sub (arr, at + i))
             (enumerate lookingFor)

fun newRecipies lookingFor arr numRecipies elf1 elf2 =
    let val x1 = Array.sub (arr, elf1)
        val x2 = Array.sub (arr, elf2)
        val ds = digits (x1 + x2)
        val () = app (fn (i, d) => Array.update (arr, numRecipies + i, d))
                     (enumerate ds)
        val numRecipies' = numRecipies + length ds
        val elf1' = (elf1 + 1 + x1) mod numRecipies'
        val elf2' = (elf2 + 1 + x2) mod numRecipies'
    in if numRecipies > 8 then
           if isItThere arr lookingFor (numRecipies - 7) then
               (numRecipies - 7, numRecipies, 6)
           else if isItThere arr lookingFor (numRecipies - 8) then
               (numRecipies - 8, numRecipies, 7)
           else
               newRecipies lookingFor arr numRecipies' elf1' elf2'
       else
           newRecipies lookingFor arr numRecipies' elf1' elf2'
    end

fun initArr maxLength =
    let val arr = Array.array (maxLength + 20, ~1)
        val () = Array.update (arr, 0, 3)
        val () = Array.update (arr, 1, 7)
    in arr end

fun solve s =
    let val arr = initArr 100000000
        val ds = map (valOf o Int.fromString) (map (implode o (fn x => [x])) (explode s))
    in newRecipies ds arr 2 0 1 end

val () =
    let val input = TextIO.inputAll (TextIO.stdIn)
    in println (Int.toString (#1 (solve input)))
    end;
