#' Locates the most (dis)similar terms in a vocabulary to a set of concepts.
#' 
#' @param term_concept_similarity a term by concept matrix computed using 
#'   computeTermConceptSimilarity()
#' @param decreasing logical denoting whether to search for most (TRUE) vs.
#'   least (FALSE) similar terms
#' @param n integer denoting the number of most (dis)similar terms to find for
#'   each concept
#' @return a list of the most (dis)similar terms for each concept, with one
#'   element for each concept, containing a named vector of similarities
#' @export
findTopTerms = function (term_concept_similarity, decreasing = TRUE, n = 10) {
  
  top_list = list()
  
  for (concept_name in colnames(term_concept_similarity)) {
    
    term_order = order(
      x = term_concept_similarity[, concept_name],
      decreasing = decreasing
    )
    term_order = term_order[1:n]
    
    sorted_terms = term_concept_similarity[term_order, concept_name]
    names(sorted_terms) = rownames(term_concept_similarity)[term_order]
    
    top_list[[concept_name]] = sorted_terms
    
  }
  
  return(top_list)
  
}