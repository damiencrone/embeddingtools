#' Loads a pre-trained word embedding model (e.g., GloVe).
#' 
#' @param file character with path to model
#' @param max_vocab_size optional integer specifying the number of terms
#'   to read
#' @return a term by dimension matrix containing the vector representation of
#'   each term in the vocabulary
#' @export
loadModel = function (file, max_vocab_size = NULL) {
  
  message("Extracting vocab")
  model_vocab = sub(
    pattern = " .*",
    replacement = "",
    x = readLines(
      con = file,
      n = max_vocab_size
    )
  )
  
  message("Extracting dimensions")
  model_text = readLines(
    con = file,
    n = max_vocab_size
  )
  
  model = t(
    sapply(
      X = strsplit(x = model_text, split = " "),
      FUN = function(x) as.numeric(x[-1])
    )
  )
  
  rownames(model) = model_vocab
  
  message(
    paste(
      "Model loaded with", prettyNum(nrow(model), big.mark = ","), "word vocab and",
      prettyNum(ncol(model), big.mark = ","), "dimensions, occupying",
      format(object.size(model), units = "auto"), "of memory"
    )
  )
  
  return(model)
  
}