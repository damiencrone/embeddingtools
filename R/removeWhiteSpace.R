#' Removes unnecessary whitespace from a character vector of texts.
#' 
#' @param text_vec character vector of texts to process
#' @return character vector of processed texts
#' @export
removeWhiteSpace = function (text_vec) {
  
  text_vec = gsub(pattern = "\\s+", replacement = " ", x = text_vec)
  start_white = startsWith(x = text_vec, prefix = " ")
  text_vec[start_white] = substr(x = text_vec[start_white], start = 2, stop = nchar(text_vec[start_white]))
  end_white = endsWith(x = text_vec, suffix = " ")
  text_vec[end_white] = substr(x = text_vec[end_white], start = 1, stop = nchar(text_vec[end_white])-1)
  
  return(text_vec)
  
}