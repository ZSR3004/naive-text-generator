
structure TreeDict : DICT =
struct

  fun log2 (n : int) : int = 
      case n of 
          0 => 0 (* hack *)
        | 1 => 1
        | _ => 1 + log2 (n div 2)

  datatype ('k, 'v) tree =
      Empty
    | Node of ('k, 'v) tree * ('k * 'v) * ('k, 'v) tree

  type ('k,'v) dict = ('k, 'v) tree 

  val empty = Empty

  fun size t =
        case t of
            Empty => 0
          | Node(l,_,r) => 1 + size l + size r
      
  fun insert (cmp, d, (k, v)) =
    case d of
      Empty => Node (empty, (k,v), empty)
    | Node (L, (k', v'), R) =>
      case cmp (k,k') of
        EQUAL => Node (L, (k, v), R)
      | LESS => Node (insert (cmp, L, (k, v)), (k', v'), R)
      | GREATER => Node (L, (k', v'), insert (cmp, R, (k, v)))

  fun lookup (cmp, d, k) =
    case d of
      Empty => NONE
    | Node (L, (k', v'), R) =>
      case cmp (k,k') of
        EQUAL => SOME v'
      | LESS => lookup (cmp, L, k)
      | GREATER => lookup (cmp, R, k)

  fun toString (kvts, d) =
      case d of
          Empty => ""
        | Node(l,kv,r) => toString (kvts, l) ^ " " ^ kvts kv ^ " " ^ toString (kvts, r)

  fun lookup' (cmp : 'k * 'k -> order, d, k) = case (lookup (cmp, d, k)) of NONE => raise Fail "key not found in dictionary" | SOME v => v
      

  (* TASK: copy your split and merge here and rename merge to merge' *)

(* Purpose: Takes a dictionary and key, then returns two trees with all the key-value pairs that are
  *          less than k (lt) and all the values that are greater than k (gt) in addition to the values
  *          that are in the pair equal to k (v') in the following order: (lt, v', gt).
*)
fun splitAt(cmp, d, k) = 
  case d of
      Empty => (Empty, NONE, Empty)
    | Node(l, (k2, v2), r) => case (cmp(k, k2)) of
          LESS => let val (ll, v', lr) = splitAt(cmp, l, k)
                      in (ll, v', Node(lr, (k2, v2), r))
                    end
        | EQUAL => (l, SOME v2, r)
        | GREATER => let val (rl, v', rr) = splitAt(cmp, r, k)
                      in (Node(l, (k2, v2), rl), v', rr)
                    end

(* Purpose: Takes in two dictionaries and merges them, combining key-value pairs if they are present in both
  *          dictionaries.
*)
fun merge' (cmp, combine, d1, d2) = 
  case d1 of
      Empty => d2
    | Node(l1, (k1, v1), r1) => let
        val (lt, v', gt) = splitAt(cmp, d2, k1)
        in case v' of
            NONE => Node(merge'(cmp, combine, l1, lt), (k1, v1), merge'(cmp, combine, r1, gt))
          | SOME x => Node(merge'(cmp, combine, l1, lt), (k1, combine(v1, x)), merge'(cmp, combine, r1, gt))
        end

  (* optimize inserts: if merging with a 1-element dictionary, insert instead, because it only needs to walk down one path of the tree *)

  fun insertWith (cmp : 'k * 'k -> order, c : 'v * 'v -> 'v, d : ('k,'v) dict, (k : 'k, v : 'v)) : ('k,'v) dict =
    case d of
      Empty => Node (empty, (k,v), empty)
    | Node (L, (k', v'), R) =>
      case cmp (k,k') of
          EQUAL => Node (L, (k, (c(v,v'))), R)
        | LESS => Node (insertWith (cmp, c, L, (k, v)), (k', v'), R)
        | GREATER => Node (L, (k', v'), insertWith (cmp, c, R, (k, v)))

  fun merge (cmp : 'k * 'k -> order, c : 'v * 'v -> 'v, d1 : ('k,'v) dict , d2 : ('k,'v) dict) : ('k,'v) dict = 
      case d1 of
          Node(Empty, kv1, Empty) => insertWith (cmp, c, d2, kv1)
        | _ => case d2 of
                 Node(Empty, kv2, Empty) => insertWith (cmp, c, d1, kv2)
               | _ => merge' (cmp, c, d1,d2)

  (* TASK: copy toSeq here *)
(* Purpose: Turns a dictionary into a sequence.
 *)
fun toSeq(d) = 
  case d of
    Empty => Seq.empty()
  | Node(l, x, r) => Seq.append(toSeq(l), Seq.append(Seq.singleton(x), toSeq(r)))

  fun map (f, d) = 
      case d of
          Empty => Empty
        | Node(l,(k,v),r) => Node (map (f, l) , (k, f v) , map (f, r))

                     
end

structure Dict :> DICT = TreeDict
