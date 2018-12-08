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

datatype tree = Node of tree list * int list

val fromString = valOf o Int.fromString

fun parseMetadata acc 0 toks = (rev acc, toks)
  | parseMetadata acc n [] = raise Fail "parseMetadata"
  | parseMetadata acc n (tok :: toks) =
    parseMetadata (tok :: acc) (n - 1) toks

fun parseNode (numChildNodes :: numMeta :: toks) =
    let val (childNodes, toks) = parseChildNodes [] numChildNodes toks
        val (meta, toks) = parseMetadata [] numMeta toks
    in (Node (childNodes, meta), toks) end
  | parseNode _ = raise Fail "parseNode"

and parseChildNodes acc 0 toks = (rev acc, toks)
  | parseChildNodes acc n [] = raise Fail "parseChildNodes"
  | parseChildNodes acc n toks =
    let val (childNode, toks) = parseNode toks
    in parseChildNodes (childNode :: acc) (n - 1) toks
    end

fun parse s =
    let val toks = map fromString (String.fields (fn c => c = #" ") s)
    in case parseNode toks of
           (node, []) => node
         | (node, _) => raise Fail "unparsed data"
    end

fun sumNodes (Node (children, metadata)) =
    foldl op+ 0 metadata + foldl op+ 0 (map sumNodes children)

val () =
    let val input = TextIO.inputAll (TextIO.stdIn)
    in println (Int.toString (sumNodes (parse input)))
    end;
