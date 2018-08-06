#' Computes the semantic similarity between a set of texts and a set of concepts
#' based on their respective vector representations.
#' 
#' @param model a term by dimension matrix containing model loaded using
#'   loadModel()
#' @param concept_vec a concept by dimension matrix containing the vector 
#'   representation of each concept in the dictionary produced by 
#'   computeConceptRepresentation()
#' @return cosine similarities between each pair of terms and concepts
#' @export
computeTermConceptSimilarity = function (model, concept_vec) {
  
  similarity_mat = computeMatrixSimilarity(
    x = model,
    y = concept_vec
  )
  
  return(similarity_mat)
  
}
