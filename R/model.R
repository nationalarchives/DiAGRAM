# hidden constant in package
.user_nodes = c(
  "Technical_Skills", "System_Security", "Checksum",
  "Info_Management", "Digital_Object", "Storage_Medium",
  "Rep_and_Refresh", "Op_Environment", "Physical_Disaster"
)

.node_map = setNames(
  c("Operating Environment", "Integrity", "System Security", "Information Management", "Storage Medium", "Replication and Refreshment", "Digital Object", "Content Metadata", "Technical Metadata", "File format", "Checksum", "Obsolescence", "Tools to Render", "Intellectual Control", "Conditions of Use", "Renderability", "Bit Preservation", "Identity", "Physical Disaster", "Storage Life", "Technical Skills"),
  c("Op_Environment", "Integrity", "System_Security", "Info_Management",
    "Storage_Medium", "Rep_and_Refresh", "Digital_Object", "Content_Metadata",
    "Tech_Metadata", "File_Format", "Checksum", "Obsolescence", "Tools_to_Render",
    "Intellectual_Control", "Conditions_of_Use", "Renderability",
    "Bit_Preservation", "Identity", "Physical_Disaster", "Storage_Life",
    "Technical_Skills")
)

.reverse_node_map = setNames(
  c("Op_Environment", "Integrity", "System_Security", "Info_Management",
    "Storage_Medium", "Rep_and_Refresh", "Digital_Object", "Content_Metadata",
    "Tech_Metadata", "File_Format", "Checksum", "Obsolescence", "Tools_to_Render",
    "Intellectual_Control", "Conditions_of_Use", "Renderability",
    "Bit_Preservation", "Identity", "Physical_Disaster", "Storage_Life",
    "Technical_Skills"),
  c("Operating Environment", "Integrity", "System Security", "Information Management", "Storage Medium", "Replication and Refreshment", "Digital Object", "Content Metadata", "Technical Metadata", "File format", "Checksum", "Obsolescence", "Tools to Render", "Intellectual Control", "Conditions of Use", "Renderability", "Bit Preservation", "Identity", "Physical Disaster", "Storage Life", "Technical Skills")
)


.custom_scoring_functions = list(
  # "Physical_Disaster" = function(response) {
  #   c(`Very Low` = 0.05, Low = 0.5, Medium = 2, High = 5)[response]
  # }
)


#' Calculate utility
#'
#' Calculate the utility metrics Intellectual Control and
#' Renderability from the given bayesian network model
#'
#' @importFrom bnlearn as.grain
#' @importFrom gRain querygrain
#' @param model a model of class bn.fit
#' @return A named list of two scores
#' @export
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

#' Score model
#'
#' This function will take a set of user values as a tibble and return the
#' utility scores from the model given those inputs
#'
#' @param model a model of class bn.fit
#' @param responses a \code{tibble(node, response)} where node is the
#' name of the node in the network and response is the user value(s) from the
#' associated question
#' @param scoring_funcs a named list of scoring functions for non numeric response types, the name should correspond to
#' the element for which a conversion needs to take place
#' @importFrom purrr map2 map
#' @importFrom stats setNames
#' @return a named list of two scores
#' @export
score_model = function(model, responses, scoring_funcs) {
  # model_clone = model
  # take names from model (may be able to remove at some point)

  # translate non numeric scores

  if(!response_valid(responses)){
    return()
  }
  prob_names = model[responses$node] %>% purrr::map(~names(.x$prob))

  # build out the non standard scoring


  # rescale to probabilities
  probs = purrr::map2(responses$response, prob_names, function(resp, name) {
    if(length(resp) == 1) {
      intermediate = c(resp, 100-resp)
    }else{
      intermediate = resp
    }
    as.table(stats::setNames(intermediate/100, name))
  }) %>% stats::setNames(responses$node)
  # print(probs)
  # update model
  for(node in responses$node) {
    model[[node]] = probs[[node]]
  }
  # return score
  calculate_utility(model)
}

make_scoring_functions = function(question_data) {
  # count how many parts there are to a question
  q_clone = question_data
  nodes = purrr::map_chr(q_clone, 'node')
  node_counts = table(nodes) %>% as.list()

  # for the multi part questions
  # TODO need to think a bit more about how to combine these funcs
  # is_multipart = node_counts > 1
  # funcs = list()
  # for(i in seq_along(is_multipart)) {
  #   if(is_multipart[i]) {
  #     # which ones
  #     ix = which(nodes == names(is_multipart[i]))
  #     purrr::map(q_clone[]
  #   }
  # }

  purrr::map(question_data, function(question) {
    if(question$type == "multiple choice") {
      option_val = setNames(question$weights, question$options)
      function(response) {
        sum(option_val[response])
      }
    } else if(question$type == "non-numeric slider") {
      option_val = setNames(unlist(question$weights), question$options)
      function(response) {
        option_val[response]
      }
    } else{
      identity
    }

  }) %>% setNames(nodes)
}

response_valid = function(responses) {
  res = purrr::map_lgl(responses, function(x) {
    valid = TRUE
    if(length(x) == 3) {
      valid = sum(x) == 100
    }
    valid
  })
  all(res, na.rm = TRUE)
}

# Function updates probability tables of model with user inputs
#' @importFrom dplyr mutate
#' @importFrom stats xtabs
update_probability <- function(node, model.probability.df, input.probability.df){
  # collect node states and corresponding probabilities
  node_states <- input.probability.df$state
  state_probabilities <- input.probability.df$probability
  # iterate through states and update model probability table
  i <- 1
  for (state in node_states){
    current.probability <- state_probabilities[i]
    model.probability.df <- model.probability.df %>%
      dplyr::mutate(Freq=ifelse(model.probability.df[[node]]==state, current.probability, .data$Freq))
    i <- i + 1
  }
  # normalise probability range between 0 and 1
  model.probability.df <- model.probability.df %>% dplyr::mutate(Freq=.data$Freq/100)
  # convert from data.frame to table
  model.probability.table <- stats::xtabs(Freq~., model.probability.df)
  return(model.probability.table)
}

