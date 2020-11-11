diagramNAT_dependencies = function(){
  htmltools::htmlDependency(
    name = "diagramNAT-assets",
    version = "0.1",
    package = "diagramNAT",
    src = "assets",
    script = "js/reactable_supplement.js"
  )
}


start_input_listener = function(el_id, input_id) {
  shinyjs::runjs(
    paste0(
      glue::glue("
waitForEl('#{el_id}', (el) => "), "{
  ", glue::glue("jQuery('#{el_id}').on('change', 'input', function(e) "), "{
    reactableSendInputMessage(e, ", glue::glue("'{input_id}');"),"
  })
})"
    )

  )
}
