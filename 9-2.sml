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

type 'a ring = { left : 'a list, right : 'a list }

fun singleton x : 'a ring = { left = [], right = [x] }

fun current { left, right = [] } = current { left = [], right = rev left }
  | current { right = (x :: xs), ... } = x

fun shiftRight { left, right = [] } i = shiftRight { left = [], right = rev left } i
  | shiftRight ring 0 = ring
  | shiftRight { left, right = x :: xs } i =
    shiftRight { left = x :: left, right = xs } (i - 1)

fun shiftLeft { left = [], right } i = shiftLeft { left = rev right, right = [] } i
  | shiftLeft ring 0 = ring
  | shiftLeft { left = x :: xs, right } i =
    shiftLeft { left = xs, right = x :: right } (i - 1)

fun insert ring x  =
    let val { left, right } = shiftRight ring 2
    in { left = left, right = x :: right } end

fun pop { left, right = [] } = pop { left = [], right = rev left }
  | pop { left, right = x :: xs } = { left = left, right = xs }

fun toList { left, right } = right @ rev left

fun playerTurn ring marbleNum =
    if marbleNum mod 23 = 0 then
        let val shifted = shiftLeft ring 7
        in (marbleNum + current shifted, pop shifted ) end
    else
        (0, insert ring marbleNum)

fun shiftToZero ring =
    let fun aux { left, right = [] } =
            aux { left = [], right = rev left }
          | aux (ring as { left, right = 0 :: _ }) = ring
          | aux { left, right = x :: xs } =
            aux { left = x :: left, right = xs }
    in (current ring, toList (aux ring)) end

fun play maxMarbles numPlayers =
    let fun aux (playerScores : int Array.array) playerNum marbleNum ring  =
            if marbleNum > maxMarbles then
                playerScores
            else
                let val (score, ring') = playerTurn ring marbleNum
                    val () = Array.update
                                 (playerScores, playerNum,
                                  Array.sub (playerScores, playerNum) + score)
                in aux playerScores
                       ((playerNum + 1) mod numPlayers)
                       (marbleNum + 1)
                       ring'
                end
    in Array.foldli (fn (i, score, acc) => (i + 1, score) :: acc)
                    []
                    (aux (Array.array (numPlayers, 0))
                         0
                         1
                         (singleton 0))
    end

fun highscore ((_, x) :: xs) = foldl Int.max x (map #2 xs)
  | highscore _ = raise Fail "highscore"

fun solve s =
    let val toks = String.tokens (fn c => c = #" ") s
    in highscore (play (valOf (Int.fromString (List.nth (toks, 6))) * 100)
                       (valOf (Int.fromString (List.nth (toks, 0)))))
    end

val () =
    let val input = TextIO.inputAll (TextIO.stdIn)
    in println (Int.toString (solve input))
    end;
