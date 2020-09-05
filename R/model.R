#' Calculate utility
#' 
#' Calculate the utility metrics Intellectual Control and 
#' Renderability from the given bayesian network model
#' 
#' @importFrom bnlearn as.grain
#' @importFrom gRain querygrain
#' @param model a model of class bn.fit
#' @return A named list of two scores
calculate_utility = function(model) {
  # function which caluclates utility
  # convert model to grain object
  if(!is_grain(model)) {
    model_grain = bnlearn::as.grain(model)
  }
  else {
    model_grain = model
  }
  # find probability of Intellectual_Control and Renderability
  query_results = gRain::querygrain(model_grain, nodes=c("Intellectual_Control", "Renderability"))
  # Extract probabilities
  prob_intellectual_control = as.numeric(query_results$Intellectual_Control["Yes"])
  prob_renderability = as.numeric(query_results$Renderability["Yes"])
  utility = list(
    "Intellectual_Control" = prob_intellectual_control,
    "Renderability" = prob_renderability
  )
  return(utility)
}
