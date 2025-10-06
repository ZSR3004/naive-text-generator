CM.make "sources-generate.cm";
val g = Generate.make_generator(Generate.open_file_no_label "data/RCV1.medium_train.txt");
val g = Generate.make_generator(Generate.open_file_no_label "data/RCV1.small_train.txt");
val test = Seq.tolist (Generate.generate (g, 7, SeqUtils.words "The"));

use "regexp.sml";

fun analyze_docs(docs : document MR.mapreducable) : ((((string * string) * string), int) Dict.dict) =
    let 
        val single_freq = ExtractCombine.extractcombine(
          String.compare,
          fn x => Seq.map(fn y => ("a", 1), x),
          Int.+,
          docs)

        fun zip_freq(x) = 
            let
                val s1 = Seq.take(Seq.length(x) - 1, x)
                val s2 = Seq.drop(0, x)
                val s3 = Seq.drop(1, x)
            in Seq.zip(Seq.zip(s1, s2), s3)
            end

        val tri_freq = ExtractCombine.extractcombine(
            www_cmp,
            fn x => Seq.map(fn y => (y, 1), zip_freq x),
            Int.+,
            docs)
        
    in tri_freq
    end

fun make_generator(docs : document MR.mapreducable) : string Seq.seq -> string = 
    let
        val freq = Dict.toSeq(analyze_docs(docs))
        val freq_temp = analyze_docs(docs)
        val total_docs = real(Seq.length(freq))

        fun find_pair((w1, w2), dict_seq) =
            Seq.filter(
                fn (((x,y),z), i) => case catword_cmp((w1,w2), (x,y)) of EQUAL => true | _ => false,
                dict_seq)

        fun find_tri((w1, w2, w3), dict_seq) =
            Seq.filter(
                fn (((x,y),z), i) => case www_cmp(((x,y), z), ((w1,w2), w3)) of EQUAL => true | _ => false,
                dict_seq)

        fun prob_func((w1,w2),w3) : real = 
            let
                val prob_w1w2 = (Seq.mapreduce(
                    fn (((x,y),z), i) => real(i),
                    0.0,
                    Real.+,
                    find_pair(((w1, w2), freq)))) / total_docs

                val prob_all = (Seq.mapreduce(
                    fn (((x,y),z), i) => real(i),
                    0.0,
                    Real.+,
                    find_tri((w1, w2, w3), freq))) / total_docs

            in prob_w1w2 / prob_all
            end 

    (* just like do this thingy. yeah you're so good bro. *)

    (* 
        You need to have a single and a pair prob_func basically. In the case that the pair
        "the false" does not appear, you should treat it like the only word there is "the" 
        and predict based on that.
    *)

    in fn x => 
    case (Seq.length(x)>=5) of
        true => let 
                val ws = find_pair(
                            (Seq.nth(Seq.length(x) - 1, x), 
                            Seq.nth(Seq.length(x) - 2, x)),
                            freq)

                val temp1 = Seq.map(
                    fn (((a,b), c), i) => (c, prob_func((a,b),c)),
                    ws)
                
                (* val (c,i) = Seq.nth(0, temp1) *)
            in Int.toString(Seq.length(temp1))
            end
        | false => "false"
    end *)

(* fun make_generator(docs : document MR.mapreducable) : string Seq.seq -> string =
    let
        (* Helper for frequency dictionary creation. *)
        fun zip_freq(x) = 
            let
                val s1 = Seq.take(Seq.length(x) - 1, x)
                val s2 = Seq.drop(0, x)
                val s3 = Seq.drop(1, x)
            in Seq.zip(Seq.zip(s1, s2), s3)
            end

        val freq = Dict.toSeq(ExtractCombine.extractcombine(
            www_cmp,
            fn x => Seq.map(fn y => (y, 1.0), zip_freq x),
            Real.+,
            docs))
        val total_phrases = Seq.mapreduce(
                    fn (((x,y),z), i) => i,
                    0.0,
                    Real.+,
                    freq)

        fun find_all(seqCmpBool, dict_seq) = Seq.filter(
                fn (((x,y),z), i) => case seqCmpBool(((x,y),z)) of EQUAL => true | _ => false, 
                dict_seq)

        fun www_prob_func(ws, bitri) = 
            let
                val prob_all = Seq.mapreduce(
                    fn (((x,y),z), i) => i,
                    0.0,
                    Real.+,
                    find_all(
                        fn (((x,y),z)) => www_cmp(((Seq.nth(0, ws), Seq.nth(1, ws)), Seq.nth(2, ws)), ((x,y),z)),
                        bitri)) / total_phrases
                val prob_w1w2 = Seq.mapreduce(
                    fn (((x,y),z), i) => i,
                    0.0,
                    Real.+,
                    find_all(
                        fn (((x,y),z)) => catword_cmp((Seq.nth(0, ws), Seq.nth(1, ws)), (x,y)),
                        bitri)) / total_phrases
            in prob_all / prob_w1w2
            end

        fun ww_prob_func(ws, bitri) =
            let
                val prob_w1w2 = Seq.mapreduce(
                    fn (((x,y),z), i) => i,
                    0.0,
                    Real.+,
                    find_all(
                        fn (((x,y),z)) => catword_cmp((Seq.nth(0, ws), Seq.nth(1, ws)), (y,z)),
                        bitri)) / total_phrases
                
                val prob_w2 = Seq.mapreduce(
                    fn (((x,y),z), i) => i,
                    0.0,
                    Real.+,
                    Seq.filter(
                        fn (((x,y),z), i) => case String.compare(Seq.nth(1, ws), z) of EQUAL => true | _ => false, 
                        bitri)) / total_phrases
            in prob_w1w2 / prob_w2
            end *)



        (* fun prob_func(ws, bitri, n) =
            case n of
                  1 => Seq.mapreduce(
                            fn (((x,y),z), i) => i,
                            0.0,
                            Real.+,
                            Seq.filter(
                                fn (((x,y),z), i) => (case (String.compare(Seq.nth(0, ws), x)) of EQUAL => true | _ => false), 
                                bitri)) / total_phrases
                | 2 => ww_prob_func(ws, bitri)
                | _ => www_prob_func(ws, bitri)

    in fn prompt => 
        let
            val n = Seq.length(prompt)
            val ws = find_all(
                fn ((x,y),z) => catword_cmp((Seq.nth(n - 1, prompt), Seq.nth(n - 1, prompt)), (x,y)),
                freq)
            val ws_prob = Dict.toSeq(Seq.mapreduce(
                    fn (((a,b), c), i) => Dict.insert(  Real.compare, 
                                                        Dict.empty, 
                                                        (prob_func(prompt, ws, n), Seq.singleton(c))),
                    Dict.empty,
                    fn(x,y) => Dict.merge(Real.compare, fn(a,b) => Seq.append(a,b), x,y),
                    ws))
            val (i , c) = Seq.nth(0, ws_prob)
            val (c_0) = Seq.nth(0, c)
        in c_0
        end
    end *)

(* fun make_generator(docs : document MR.mapreducable) : string Seq.seq -> string =
    let

        fun zip_freq(x) = 
            let
                val s1 = Seq.drop(0, x)
                val s2 = Seq.drop(1, x)
                val s3 = Seq.drop(2, x)
            in Seq.zip(Seq.zip(s1, s2), s3)
            end

        val freq = Dict.toSeq(ExtractCombine.extractcombine(
            www_cmp,
            fn x => Seq.map(fn y => (y, 1.0), zip_freq x),
            Real.+,
            docs))
        val phrase = Seq.mapreduce(
                    fn (((x,y),z), i) => i,
                    0.0,
                    Real.+,
                    freq)

    
    in fn prompt => 
        let
            val temp = 0
        in Real.toString(thing)
        end
    end *)

(* fun make_generator(docs : document MR.mapreducable) : string Seq.seq -> string =
    let
        (* Purpose: Takes a sequence x and splits it into groups of three.
         * 
         * Example: zip_freq ["The", "Cute", "Cat", "Sat"] = 
         *          [(("The", "Cute"), "Cat"), (("Cute", "Cat"), "Sat")] 
        *)
        fun zip_freq(x) = 
            let
                val s1 = Seq.drop(0, x)
                val s2 = Seq.drop(1, x)
                val s3 = Seq.drop(2, x)
            in Seq.zip(Seq.zip(s1, s2), s3)
            end
        
        (* Creates a frequency sequence that marks how frequently phrases of 
         * three words appear in docs.
        *)
        val freq = Dict.toSeq(ExtractCombine.extractcombine(
            www_cmp,
            fn x => Seq.map(fn y => (y, 1.0), zip_freq x),
            Real.+,
            docs))
        
        (* Purpose: Calculates the total number of phrases in freq.
         * 
         * Example: If freq = [(p1, 2), (p2, 3)], then the total 
         *          number of phrases is 5. (p1 and p2 are just 
         *          two different random phrases of three words).
        *)
        val total_phrases = Seq.mapreduce(
                    fn (((x,y),z), i) => i,
                    0.0,
                    Real.+,
                    freq)

        fun find_all(seqCmpBool, dict_seq) = Seq.filter(
                fn (((x,y),z), i) => case seqCmpBool(((x,y),z)) of EQUAL => true | _ => false, 
                dict_seq)
        
        fun pair_cond(ws, dict) = 
            let
                val Pw1w2 = Seq.mapreduce(
                    fn (((x,y),z), i) => i,
                    0.0,
                    Real.+,
                    find_all(
                        fn (((x,y),z), i) => catword_cmp((Seq.nth(0, ws), Seq.nth(1, ws)), (x,y)),
                        dict)) / total_phrases
                val Pw1  = Seq.mapreduce(
                    fn (((x,y),z), i) => i,
                    0.0,
                    Real.+,
                    find_all(
                        fn (((x,y),z), i) => String.compare(Seq.nth(0, ws), x),
                        dict)) / total_phrases
            in Pw1w2 / Pw1
            end

        fun tri_cond(ws, dict) = 
            let
                val Pw1w2w3 = Seq.mapreduce(
                        fn (((x,y),z), i) => i,
                        0.0,
                        Real.+,
                        find_all(
                            fn (((x,y),z), i) => 
                            www_cmp(((Seq.nth(0, ws), Seq.nth(1, ws)), Seq.nth(2, ws)), ((x,y), z)),
                            dict)) / total_phrases
                val Pw1w2 = Seq.mapreduce(
                        fn (((x,y),z), i) => i,
                        0.0,
                        Real.+,
                        find_all(
                            fn (((x,y),z), i) => catword_cmp((Seq.nth(0, ws), Seq.nth(1, ws)), (x,y)),
                            dict)) / total_phrases
            in Pw1w2w3 / Pw1w2
            end
        
        fun prob_func(ws, n) =
            case n of
                  0 => 0.0
                | 1 =>
                    end
                | _ => tri_cond(ws, Seq.empty())   (* This is really just n = 2, but I wanted to get rid of the
                                                warning message. *)

    in fn prompt => 
        let
            val ws_prob = prob_func(prompt, Seq.length(prompt))
        in ""
        end
    end
