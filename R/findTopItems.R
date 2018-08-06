#' Function to locate the most (dis)similar items to a concept.
#' 
#' @param concept_similarity an item by concept matrix
#' @param decreasing logical denoting whether to search for most (TRUE) vs.
#'   least (FALSE) similar items
#' @param n integer denoting the number of most (dis)similar items to find for
#'   each concept
#' @return a list of the most (dis)similar items for each concept, with one
#'   element for each concept, containing a named vector of similarities
#' @export
findTopItems = function (concept_similarity, decreasing = TRUE, n = 10) {
  
  top_list = list()
  
  for (concept_name in colnames(concept_similarity)) {
    
    item_order = order(
      x = concept_similarity[, concept_name],
      decreasing = decreasing
    )
    item_order = item_order[1:n]
    
    sorted_items = concept_similarity[item_order, concept_name]
    names(sorted_items) = rownames(concept_similarity)[item_order]
    
    top_list[[concept_name]] = sorted_items
    
  }
  
  return(top_list)
  
}