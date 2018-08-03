#' Computes vector representations of a (set of) text(s) based on a given
#' model.
#' 
#' @param text_vec character vector of texts to process
#' @param model matrix containing GloVe model loaded using loadModel()
#' @return a text by dimension matrix containing the vector representation of
#'   each text
#' @export
computeTextVectors = function (text_vec, model) {
  
  # Split each text into individual words
  words = strsplit(x = text_vec, split = " ")
  
  # Trim model vocab to only include words in corpus
  unique_words = unique(unlist(words))
  unique_words_in_vocab = rownames(model) %in% unique_words
  model = model[unique_words_in_vocab,]
  
  # Reduce words list to words in model vocab
  words = lapply(
    X = words,
    FUN = function(x, model){
      x[x %in% rownames(model)]
    },
    model = model
  )
  
  # Check for empty entries in words
  model_wordcounts = sapply(words, length)
  if (any(model_wordcounts == 0)) {
    zero_wordcounts = which(model_wordcounts == 0)
    message(paste("Zero wordcounts found for", length(zero_wordcounts), "cases:", paste(zero_wordcounts, collapse = ", ")))
  }
  
  # For every text, get model dimensions for each word and average over words
  text_representation = sapply(
    X = words,
    FUN = function(x, model){
      if(length(x) > 1) {
        return(apply(X = model[x,], MARGIN = 2, mean))
      } else if (length(x) == 1) {
        return(model[x,])
      } else {
        return(rep(NA, ncol(model)))
      }
    },
    model = model
  )
  
  text_representation = t(text_representation)
  
  return(text_representation)
  
}