#' Computes vector representations of a (set of) concept(s) based on a given
#' model, and the terms in the model vocabulary that define that concept(s)
#' (similar to the approaches described in Garten et al. 2018, BRM).
#' 
#' @param model matrix containing model loaded using loadModel()
#' @param term_set a list with one element for each dictionary concept, with 
#'   each element containing a character vector with all terms defining the 
#'   concept, or a row/column-named dataframe with terms in the rows and weights
#'   in the columns
#' @return a concept by dimension matrix containing the vector representation of
#'   each concept in the dictionary
#' @export
computeConceptRepresentation = function (model, term_set = NULL) {
  
  # Validate dictionary format and perform dictionary format conversions if necessary
  if (class(term_set) == "list") {
    
    dictionary = data.frame(
      row.names = unique(unlist(term_set))
    )
    
    for (concept in names(term_set)) {
      
      concept_terms = term_set[[concept]]
      
      dictionary[, concept] = NA
      dictionary[, concept] = as.integer(rownames(dictionary) %in% concept_terms)
      
    }
    
  } else if (class(term_set) != "data.frame") {
    
    stop("Invalid class for term_set: Either a list or dataframe is required.")
    
  }
  
  # Verify that all terms are in the model vocabulary
  all_terms = rownames(dictionary)
  missing_terms = all_terms[!all_terms %in% rownames(model)]
  if (length(missing_terms)) {
    stop(paste("The following term(s) are not in the vocabulary:", paste(missing_terms, collapse = ", ")))
  }
  
  # Initialise dictionary concept matrix
  concept_mat = matrix(
    data = NA,
    nrow = ncol(dictionary),
    ncol = ncol(model),
    dimnames = list(
      colnames(dictionary),
      colnames(model)
    )
  )
  
  # Get vectors for all terms in dictionary
  term_model = model[rownames(dictionary),]
  
  # Loop through catgories and compute vector
  for (concept in colnames(dictionary)) {
    
    term_weights = dictionary[, concept] / sum(dictionary[, concept])
    weighted_term_model = term_model * term_weights
    
    concept_mat[concept,] = apply(
      X = weighted_term_model,
      MARGIN = 2,
      FUN = sum
    )
    
  }
  
  return(concept_mat)
  
}