#' Computes vector representations of a (set of) concept(s) based on a given 
#' model, and the terms in the model vocabulary that define that concept(s) 
#' (similar to the approaches described in Garten et al. 2018, BRM).
#' 
#' @param model a term by dimension matrix containing model loaded using 
#'   loadModel()
#' @param term_set a dataframe with the columns "concept" (naming the dictionary
#'   concepts), "root" (naming the word stems used to group the terms), "term"
#'   (listing the terms to be used to construct the concept representations),
#'   and "include" (optional logical variable denoting whether a given term
#'   should be included in the concept representation)
#' @return a concept by dimension matrix containing the vector representation of
#'   each concept in the dictionary
#' @export
computeConceptRepresentationWithWildcards = function (model, term_set, remove_missing = FALSE) {
  
  if (!is.null(term_set$include)) {
    message(paste("Removing", sum(!term_set$include), "terms marked for removal"))
    term_set = term_set[term_set$include,]
  }
  
  missing_terms = !term_set$term %in% rownames(model)
  if (any(missing_terms) & remove_missing) {
    message(paste("Removing", sum(missing_terms), "terms not present in vocab"))
    term_set = term_set[!missing_terms,]
  }
  
  root_dictionary_list = list()
  
  for (i in unique(term_set$root)) {
    
    root_ind = term_set$root == i
    terms = term_set$term[root_ind]
    
    root_dictionary_list[[i]] = terms
    
  }
  
  root_vec = computeConceptRepresentation(
    model = model,
    term_set = root_dictionary_list
  )
  
  concept_mat = matrix(
    data = NA,
    nrow = length(unique(term_set$concept)),
    ncol = ncol(model),
    dimnames = list(
      unique(term_set$concept),
      colnames(model)
    )
  )
  
  for (concept in unique(term_set$concept)) {
    root_names = unique(term_set$root[term_set$concept == concept])
    concept_mat[concept,] = colMeans(root_vec[root_names, , drop = FALSE])
  }
  
  return(concept_mat)
  
}
