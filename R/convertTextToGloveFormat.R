#' Converts text to a format consistent with the GloVe model vocabulary.
#' 
#' @param text_vec character vector of texts to process
#' @return character vector of processed texts
#' @export
convertTextToGloveFormat = function (text_vec) {
  
  text_vec = tolower(text_vec)
  
  # Separate punctuation
  punc_vec = c(".", "/", "\\", "?", "(", ")", ",", "'", '"', "-", "&", ":", ";", "!")
  for (char in punc_vec) {
    text_vec = gsub(
      pattern = paste0("[", char, "]"),
      replacement = paste0(" ", char, " "),
      x = text_vec
    )
  }
  
  # Handle some common special cases with apostrophies
  text_vec = gsub(pattern = "' s ", replacement = " 's ", x = text_vec)
  text_vec = gsub(pattern = "' re ", replacement = " 're ", x = text_vec)
  text_vec = gsub(pattern = "' ve ", replacement = " 've ", x = text_vec)
  text_vec = gsub(pattern = "' ll ", replacement = " 'll ", x = text_vec)
  text_vec = gsub(pattern = "n ' t ", replacement = " n't ", x = text_vec)
  
  return(text_vec)
  
}