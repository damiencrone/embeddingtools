#' Computes the semantic similarity between a set of texts and a set of concepts
#' based on their respective vector representations.
#' 
#' @param text_representation a text by dimension matrix containing the vector
#'   representation of each text produced by computeTextVectors()
#' @param concept_mat a concept by dimension matrix containing the vector
#'   representation of each concept in the dictionary produced by
#'   computeConceptRepresentation()
#' @return cosine similarities between each pair of texts and concepts
#' @export
computeTextConceptSimilarity = function (text_representation, concept_mat) {
  
  similarity_mat = matrix(
    data = NA,
    nrow = nrow(text_representation),
    ncol = nrow(concept_mat),
    dimnames = list(
      rownames(text_representation),
      rownames(concept_mat)
    )
  )
  
  for (concept_name in rownames(concept_mat)) {
    
    similarity_mat[, concept_name] = apply(
      X = text_representation,
      MARGIN = 1,
      FUN = lsa::cosine,
      y = concept_mat[concept_name,]
    )
    
  }
  
  return(similarity_mat)
  
}