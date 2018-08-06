#' Internal function for computing the semantic similarity between two matrices
#' of vector representations.
#' 
#' @param x an m*k matrix containing the vector representation of some entities
#' @param y an n*k matrix containing the vector representation of some entities
#' @return an m*k matrix of cosine similarities between the rows of x and y
computeMatrixSimilarity = function (x, y) {
  
  similarity_mat = text2vec::sim2(
    x = x,
    y = y,
    method = "cosine",
    norm = "l2"
  )
  
  return(similarity_mat)
  
}
