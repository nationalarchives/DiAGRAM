#' dev_banner module ui
#'
#' @param id shiny id necessary for module
#' @importFrom shiny NS tagList h3 strong p
#' @importFrom shinydashboard box
#' @export
dev_banner_module_ui = function(id){
  ns = NS(id) # no lint (excluded from lint for jrshinyapp template)
  tagList(
    # Welcome box
    shinydashboard::box(
      title = NULL,
      width = 12,
      background="green",
      shiny::h3(
        shiny::strong("Important note: This model is still in development")
      ),
      shiny::p(
        "There will be further user interface changes and additional functionality added as the
        project progresses. Any feedback to inform the future development would be welcome
        - please send your comments to a member of the project team.")
    )
  )
}

#' dev_banner module server
#'
#' @param input necessary input arg for shiny server function
#' @param output necessary output arg for shiny server function
#' @param session necessary session arg for shiny server function
#' @import shiny
dev_banner_module_server = function(input, output, session){
  ns = session$ns # no lint (excluded from lint for jrshinyapp template)
}
