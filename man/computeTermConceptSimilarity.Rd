% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/computeTermConceptSimilarity.R
\name{computeTermConceptSimilarity}
\alias{computeTermConceptSimilarity}
\title{Computes the semantic similarity between a set of texts and a set of concepts
based on their respective vector representations.}
\usage{
computeTermConceptSimilarity(model, concept_vec)
}
\arguments{
\item{model}{a term by dimension matrix containing model loaded using
loadModel()}

\item{concept_vec}{a concept by dimension matrix containing the vector 
representation of each concept in the dictionary produced by 
computeConceptRepresentation()}
}
\value{
cosine similarities between each pair of terms and concepts
}
\description{
Computes the semantic similarity between a set of texts and a set of concepts
based on their respective vector representations.
}
