
structure NaiveBayes :> NAIVE_BAYES_CLASSIFIER =
struct

    type category = string

    type labeled_document = category * string Seq.seq
    type document = string Seq.seq
        
    type statistics = 
          (category,int) Dict.dict           (* maps each category to number of documents with that category *)
        * (category,int) Dict.dict           (* maps each category to number of words in documents with that category *)
        * (category * string, int) Dict.dict (* maps each (cat,word) to frequency *)
        * category Seq.seq                   (* list of categories (no duplicates) *)
        * int                                (* total number of documents *)
        * int                                (* total number of different words *)

    (* TASK *)
    
    (* Purpose: Compares two tuples of the from (cat * str) by applying String.compare to
     *          each category. If the categories are equal, it then applies String.compare
     *          the second element of the tuple (the words).
     *)
    fun catword_cmp ((a,b), (x,y)) =
          case (String.compare(a,x)) of
              EQUAL => String.compare(b,y)
            | comparison => comparison

    (* Purpose: Returns statistics about the (training) set of documents that will
     *          be used to train/test the classifier (or otherwise used to classify
      *         documents).
    *)
    fun gather (train : labeled_document MR.mapreducable) =
      let
          val num_docs_by_cat = ExtractCombine.extractcombine(
              String.compare,
              fn (x,y) => Seq.singleton((x, 1)),
              Int.+,
              train)

          val num_words_by_cat = ExtractCombine.extractcombine(
              String.compare,
              fn (x,y) => Seq.singleton((x, Seq.length(y))),
              Int.+,
              train)

          val freqs = ExtractCombine.extractcombine(
              catword_cmp,
              fn(x,y) => Seq.map(fn z => ((x,z), 1), y),
              Int.+,
              train)

          val seq_num_docs_by_cat = Dict.toSeq(num_docs_by_cat)
          val all_categories = Seq.map(fn (x,y) => x, seq_num_docs_by_cat)
          val total_num_docs = Seq.mapreduce(fn(x,y) => y, 0, Int.+, seq_num_docs_by_cat)

          val total_num_words = Dict.size(ExtractCombine.extractcombine(
              String.compare,
              fn(x,y) => Seq.map(fn z => (z,1), y),
              Int.+,
              train))

        in (num_docs_by_cat, num_words_by_cat, freqs, all_categories, total_num_docs, total_num_words)
      end
        
    (* TASK *)
    (* Purpose: Calculates the likelhood for a given document to be in some category.
    *)
    fun possible_classifications 
        ((num_docs_by_cat,
          num_words_by_cat,
          freqs,
          all_categories, 
          total_num_docs,
          total_num_words) : statistics,
         test_doc : document) : (category * real) Seq.seq = 
        let
          fun docs_per_cat (cat) = Math.ln(
                                    real(Dict.lookup'(String.compare, 
                                    num_docs_by_cat, cat)) / real(total_num_docs))

          fun p_doc_cat(cat) = Seq.mapreduce(
            fn x => (case (Dict.lookup(catword_cmp, freqs, (cat, x))) of
                        NONE => Math.ln(1.0 / real(total_num_words))
                      | SOME v => Math.ln(real(v) / real(Dict.lookup'(String.compare, num_words_by_cat, cat)))),
            0.0,
            Real.+,
            test_doc)

          fun bayes(cat) = docs_per_cat(cat) + p_doc_cat(cat)

        in Seq.map(fn x => (x, bayes x), all_categories)
        end

    (* TASK *)
    (* Purpose: Selects the category for which a document is most likely to be in.
    *)
    fun classify (stats : statistics, test_doc : document) : (category * real) = 
      let
        val classifications = possible_classifications(stats, test_doc)
        fun prob_cmp ((cat1, prob1), (cat2, prob2)) =
          case Real.compare(prob1, prob2) of
               EQUAL => (cat1, prob1)
            |  LESS => (cat2, prob2)
            |  GREATER => (cat1, prob1)
      in Seq.reduce(prob_cmp, ("",Real.negInf), classifications)
      end

    (* TASK *)
    (* Trains the classifier to predict the categories of input documents during testing/usage.
    *)
    fun train_classifier (train : labeled_document MR.mapreducable) : document -> (category * real) =
        let 
          val stats = gather(train)
          val classification = fn (doc) => classify(stats, doc)
      in classification
      end

    (* Accuracy Statment:
     * Small, Small : 5/8
     * Medium, Medium : 680/808
     * Large, Large : 70122/78899
     * 
     * Small, Large: 3/8
     * Large, Small: 52220/78899
     * Medium, Large: 704/808
     * 
     * It seems like a larger training dataset didn't have much impact on a smaller testing set.
     * Most likely, the test set just wasn't big enough to make a difference, but the small 
     * training set had a clear impact on the results of the large test, being almost a third
     * less accurate. Finally, the large training set did in fact improve the accuracy on the
     * medium test set.
    *)
        
end
