#' Computes the semantic similarity between a set of texts and a set of concepts
#' based on their respective vector representations.
#' 
#' @param text_representation a text by dimension matrix containing the vector
#'   representation of each text produced by computeTextVectors()
#' @param concept_vec a concept by dimension matrix containing the vector
#'   representation of each concept in the dictionary produced by
#'   computeConceptRepresentation()
#' @return cosine similarities between each pair of texts and concepts
#' @export
computeTextConceptSimilarity = function (text_representation, concept_vec) {
  
  similarity_mat = computeMatrixSimilarity(
    x = text_representation,
    y = concept_vec
  )
  
  return(similarity_mat)
  
}