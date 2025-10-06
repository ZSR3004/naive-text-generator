fun testb (s : string) (n : bool) (m : bool) : unit =
    case n = m of
        true => print ("Test " ^ s ^ " OK\n")
      | false => print ("Test " ^ s ^ " FAILED:\n    Expected: " ^ Bool.toString m ^ "\n    Got: " ^ Bool.toString n  ^ "\n")

fun map (f : 'a -> 'b, l : 'a list) : 'b list = List.map f l

fun reduce (n : 'a * 'a -> 'a, e : 'a, l : 'a list) : 'a = List.foldr n e l

fun exists (f : 'a -> bool, l : 'a list) : bool = reduce(fn (x,y) => x orelse y, false, map(f,l))

fun splits(l : 'a list) : ('a list * 'a list) list =
    case l of
        [] => [([] , [])]
      | x :: xs => ([] , l) :: map(fn (p,s) => (x :: p, s), splits xs)
    
(* assumes input is non-empty *)
fun reduceNonEmpty (n : 'a * 'a -> 'a, l : 'a list) : 'a =
    case l of
        [x] => x
      | x :: xs => n(x,reduceNonEmpty(n,xs))

    
datatype regexp =
      Lit of char
    | Or of regexp * regexp
    | Then of regexp * regexp
    | OneOrMore of regexp

fun litString(s) : regexp = reduceNonEmpty(Then, map(Lit, String.explode(s)))
          
val letters : regexp = reduceNonEmpty(Or, map(Lit, String.explode "abcdefghijklmnopqrstuvwxyz"))
val suffix : regexp = reduceNonEmpty(Or, map (litString, ["com", "edu", "net", "org"]))
val email : regexp = reduceNonEmpty(Then, [OneOrMore(letters), Lit #"@", OneOrMore(letters), Lit #".", suffix])
      
fun match (r : regexp, s : char list) : bool =
    case r of
        Lit(c) => s = [c]
      | Or(r1,r2) => match(r1,s) orelse match(r2, s)
      | Then(r1,r2) => exists (fn (p1,p2) => match(r1, p1) andalso match (r2, p2), splits s)
      | OneOrMore r => match(r,s) orelse exists (fn (p1,p2) => match(r, p1) andalso match (OneOrMore r, p2), splits s)

fun testmatch() =
    (testb "m1" (match(email, String.explode "dlicata@wesleyan.edu")) true; 
     testb "m2" (match(email, String.explode "dlicata@wesleyan.eddu")) false;
     testb "m3" (match(email, String.explode "dlicatawesleyan.edu")) false;
     testb "m4" (match(email, String.explode "@wesleyan.edu")) false)


fun matches(k : regexp list, s : char list) : bool =
    case k of
        [] => (s = [])
      | r :: k' => exists (fn (p1,p2) => match(r, p1) andalso matches (k', p2), splits s)

(* 
  fastmatch(r,s,k) = exists(fn (p1,p2) => match(r,p1) andalso matches(k,p2), splits(s))
*)
fun fastmatch(r : regexp, k : regexp list, s : char list) : bool =
    case r of
        Lit(c) => (case s of
                       [] => false
                     | x :: xs =>
                           c=x andalso
                           (case k of
                               [] => (xs = [])
                             | r' :: k' => fastmatch(r', k', xs)))
      | Or(r1,r2) => fastmatch(r1,k,s) orelse fastmatch(r2,k,s)
      | Then(r1,r2) => fastmatch(r1,r2::k,s)
      | OneOrMore(r) => fastmatch(r, k, s) orelse exists (fn (p1,p2) => fastmatch(r, [], p1) andalso fastmatch (OneOrMore r, k, p2), splits s)

fun testfastmatch() =
    (testb "fm1" (fastmatch(email, [], String.explode "dlicata@wesleyan.edu")) true; 
     testb "fm2" (fastmatch(email, [], String.explode "dlicata@wesleyan.eddu")) false;
     testb "fm3" (fastmatch(email, [], String.explode "dlicatawesleyan.edu")) false;
     testb "fm4" (fastmatch(email, [], String.explode "@wesleyan.edu")) false)
