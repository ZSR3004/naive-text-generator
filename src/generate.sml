structure Generate =
struct

(* NOTE:

    For some reason, I could not get the random number generator to work even after I redownloaded
    all the files. In the predict_for_one and predict_for_two function, there is a variable
    r that should be r = Randomreal(), but since I couldn't get it to work, I placed a random real
    number between 0 and 1 to cope. Changing those two lines would allow the generator to select
    words randomly, giving weight to those with higher probabilities (since the selecting part
    is its own function).

    Also, just to flag for the prompt "The", it repeats itself which would have been fixed if 
    the real number generator was working (it hits "and currency", then keeps predicting based
    on that phrase, so it outputs the same thing everytime).

*)

    type document = string Seq.seq
        
    fun open_file_no_label (s : string) : document FileMR.mapreducable  =
        (fn () => TextIO.openIn s, fn x => case TestFile.parse_line x of NONE => NONE | SOME (x,y) => SOME y)

    (* val r = Random.rand (Int.fromLarge (Time.toSeconds(Time.now())),2438844)

    (* Generate a random number in the range 0.0 though 1.0.  
       Each time you call the function, it produces a different number.  
       The numbers produced will be different every time you compile the program.  
       *)
    fun randomReal() = Random.randReal r *)

     (* Purpose: Compares two tuples of the from (cat * str) by applying String.compare to
     *          each category. If the categories are equal, it then applies String.compare
     *          the second element of the tuple (the words).
     *)
    fun catword_cmp ((a,b), (x,y)) =
          case (String.compare(a,x)) of
              EQUAL => String.compare(b,y)
            | comparison => comparison

    fun www_cmp (((a,b),c), ((x,y),z)) =
        case (String.compare(a,x)) of
              EQUAL => catword_cmp((b,c), (y,z))
            | result => result
    
    fun make_generator(docs : document MR.mapreducable) : string Seq.seq -> string =
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
            val freq = ExtractCombine.extractcombine(
                www_cmp,
                fn x => Seq.map(fn y => (y, 1.0), zip_freq x),
                Real.+,
                docs)
            
            val freq_seq = Dict.toSeq(freq)
            
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
                        freq_seq)

            (* This function is equivalent to P(w2 | w1). In paticular, it uses the
             * P(w1 INTERSECT w2) / P(w2) definition.
            *)
            fun bi_cond(w2, w1, freq_w1, dict) = 
                let
                    val pw1w2 = Seq.mapreduce(
                        fn (((x,y),z), i) => i,
                        0.0,
                        Real.+,
                        Seq.filter(fn (((x,y),z), i) => catword_cmp((w1, w2), (x,y)) = EQUAL, dict)) / total_phrases
                    val pw1 = freq_w1 / total_phrases
                in pw1w2 / pw1
                end

            (* This function is equivalent to P(w3 | w1 INTERSECT w2). In paticular, it uses the
             * P(w1 INTERSECT w2 INTERSECT w3) / P(w1 INTERSECT w2) definition.
            *)
            fun tri_cond(w3, w2, w1, freq_w1w2, dict) =
                let
                    val pw1w2w3 = Seq.mapreduce(
                            fn (((x,y),z), i) => i,
                            0.0,
                            Real.+,
                            Seq.filter(fn (((x,y),z), i) => www_cmp(((w1, w2), w3), ((x, y), z)) = EQUAL, dict)
                            ) / total_phrases
                    val pw1w2 = freq_w1w2 / total_phrases
                in pw1w2w3 / pw1w2
                end

            (* Selects a word based on its probability of occuring. It is designed to
             * also select less common words.
            *)
            fun select_word(ws, r, acc, index) = 
                case (Seq.length(ws) <= index) of
                      true => let val (w,p) = Seq.nth(Seq.length(ws) - 1, ws)
                                    in w end
                    | false =>  let 
                                    val (w, p) = Seq.nth(index, ws)
                                    val new_acc = acc + p
                                in 
                                    case (r <= new_acc) of
                                      true => w
                                    | false => select_word(ws, r, new_acc, index + 1)
                                end
            
            (* Predicts a word based on the previous word. *)
            fun predict_for_one(prompt, n) =
                let
                            val ws = Seq.filter(
                                        fn (((x,y),z), i) => String.compare(Seq.nth(n - 1, prompt), x) = EQUAL, 
                                        freq_seq)
                            val freq_w1 = Seq.mapreduce(
                                        fn (((x,y),z), i) => i,
                                        0.0,
                                        Real.+,
                                        ws)
                            val ws_prob = Seq.map(fn (((x,y),z), i) => (y, bi_cond(y,x, freq_w1, ws)), ws)
                            val r = 0.85
                        in select_word(ws_prob, r, 0.0, 0)
                        end

            (* Predicts a word based on the previous TWO words. *)
            fun predict_for_two(prompt, n, ws, freq_w1w2) = 
                let
                    val ws_prob = Seq.map(fn (((x,y),z), i) => (y, tri_cond(z, y, x, freq_w1w2, ws)), ws)
                    val r = 0.97
                in select_word(ws_prob, r, 0.0, 0)
                end

        in fn prompt => 
            let val n = Seq.length(prompt) in
            case (n) of
                  0 => "ERROR: EMPTY PROMPT"
                | 1 => predict_for_one(prompt, n)
                | _ => let 
                            val ws = Seq.filter(
                                fn (((x,y),z), i) => catword_cmp((Seq.nth(n - 1, prompt), Seq.nth(n - 2, prompt)), (x,y)) = EQUAL,
                                freq_seq)
                            val freq_w1w2 = Seq.mapreduce(
                                fn (((x,y),z), i) => i,
                                0.0,
                                Real.+,
                                ws)
                        in case (Real.compare(freq_w1w2,0.0) = EQUAL) of
                              true => predict_for_one(prompt, n)
                            | false => predict_for_two(prompt, n, ws, freq_w1w2)
                        end
                end
        end

    (* Generate the next n words of the prompt using the generator g, and
       return the prompt with the generated words added to the end.  The
       prompt should be a sequence of words.  *)
    fun generate (g : string Seq.seq -> string, n : int, prompt : string Seq.seq) : string Seq.seq =
        case n of
            0 => prompt
          | _ => generate(g, n-1, Seq.append(prompt, Seq.singleton (g prompt)))


end