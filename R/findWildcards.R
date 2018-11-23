#' Perform wildcard search for terms in a model's vocabulary.
#' 
#' @param x a character vector of words
#' @param model matrix containing GloVe model loaded using loadModel()
#' @return a list with one element per item in x, containing wildcard search
#'   results
#' @export
findWildcards = function (x, model) {
  
  vocab = rownames(glove)
  nchar_vec = nchar(x)
  
  # Identify wildcard items and wildcard types
  sw = substr(x = x, start = nchar_vec, stop = nchar_vec) == "*"
  ew = substr(x = x, start = 1, stop = 1) == "*"
  
  contains = sw & ew
  sw_only = sw & !ew
  ew_only = ew & !sw
  no_wc = !grepl(pattern = "[*]", x = x)
  
  if (all(no_wc)) {
    warning("No wildcards found (wildcards should start and/or end with an *)")
  }
  
  # Initialise result list
  result_list = lapply(
    X = x,
    FUN = function(x){return(list())}
  )
  names(result_list) = x
  result_list[no_wc] = x[no_wc]
  
  x_root = gsub(pattern = "[*]", replacement = "", x = x)
  
  if (any(sw_only)) {
    sw_list = lapply(
      X = x_root[sw_only],
      FUN = function (pref, vocab) {
        return(vocab[startsWith(x = vocab, prefix = pref)])
      },
      vocab = vocab
    )
    result_list[sw_only] = sw_list
  }
  
  if (any(ew_only)) {
    ew_list = lapply(
      X = x_root[ew_only],
      FUN = function (suf, vocab) {
        return(vocab[endsWith(x = vocab, suffix = suf)])
      },
      vocab = vocab
    )
    result_list[ew_only] = ew_list
  }
  
  if (any(contains)) {
    con_list = lapply(
      X = x_root[contains],
      FUN = function (pat, vocab) {
        return(vocab[grepl(pattern = pat, x = vocab)])
      },
      vocab = vocab
    )
    result_list[contains] = con_list
  }
  
  return(result_list)
  
}