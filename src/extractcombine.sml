
structure ExtractCombine :> EXTRACT_COMBINE =
struct

    (* Purpose: Takes a sequence and turns it into a dictionary. The higher
     *          order function "extract" dictates how each element of the
     *          sequence will be transformed when placed into the dictionary.
    *)
    fun extractcombine (cmp, extract, combine, s) = 
        MR.mapreduce(
                fn x => Seq.mapreduce(
                            fn(x,y) => Dict.insert(cmp, Dict.empty, (x,y)),
                            Dict.empty,
                            fn(x,y) => Dict.merge(cmp, combine, x, y),
                            extract(x)),
                Dict.empty,
                fn (x,y) => Dict.merge(cmp, combine, x, y),
                s)
end

