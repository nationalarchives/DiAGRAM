#' policy_visualisation module ui
#'
#' @param id shiny id necessary for module
#' @importFrom shiny NS tagList
#' @export
policy_visualisation_module_ui = function(id){
  ns = NS(id) # no lint (excluded from lint for jrshinyapp template)
  tagList(
  )
}

#' policy_visualisation module server
#'
#' @param input necessary input arg for shiny server function
#' @param output necessary output arg for shiny server function
#' @param session necessary session arg for shiny server function
#' @import shiny
policy_visualisation_module_server = function(input, output, session, model_data){
  ns = session$ns # no lint (excluded from lint for jrshinyapp template)
  # model_data() is reactive
}
