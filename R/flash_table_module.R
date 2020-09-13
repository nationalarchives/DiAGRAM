# old_val = tibble(x = 12, y = 15)

flash_table_module_ui = function(id, original_score) {
  ns = shiny::NS(id)
  table_data = tibble::as_tibble(original_score) %>%
    tidyr::gather("Score", "Value") %>%
    dplyr::mutate(Value = round(.data$Value, 2))
  shiny::tagList(
    shinyjs::useShinyjs(),
    build_table(table_data, id = ns('flash-table'))
  )
}
flash_table_module_server = function(input, output, session, original_score, new_score) {
  ns = session$ns

  old_state = shiny::reactiveVal(NULL)

  shiny::observeEvent(original_score(), {
    print("orig score")
    inter = unlist(original_score())
    print(inter)
    print(inter["Intellectual_Control"])
    # setNames(inter$Value, inter$Score)
    update_table_val_js("#{ns('flash-table')} tbody > tr:nth-child(1) > td:nth-child(2)", inter["Intellectual_Control"])
    update_table_val_js("#{ns('flash-table')} tbody > tr:nth-child(2) > td:nth-child(2)", inter["Renderability"])
    old_state(inter)
  })

  observe({
    print("old state")
    print(old_state())
  })

  shiny::observeEvent(new_score(), {
    # req(!is.null(old_state()))

    new = unlist(new_score())
    print("new score")
    print(new)
    # print(old_state())
    old = old_state()[names(new)]
    # find the differences
    bigger = which(new > old)
    smaller = which(new < old)
    ## need to update the table values as well
    bigger_selectors = glue::glue("#{ns('flash-table')} tbody > tr:nth-child({bigger}) > td:nth-child(2)")
    smaller_selectors = glue::glue("#{ns('flash-table')} tbody > tr:nth-child({smaller}) > td:nth-child(2)")
    purrr::walk2(new[bigger], bigger_selectors, ~shinyjs::runjs(update_table_val_js(.y, round(.x, 2))))
    purrr::walk2(new[smaller], smaller_selectors, ~shinyjs::runjs(update_table_val_js(.y, round(.x, 2))))
    purrr::walk(bigger_selectors, ~shinyjs::runjs(colour_flash_js(.x, "green")))
    purrr::walk(smaller_selectors, ~shinyjs::runjs(colour_flash_js(.x, "red")))

    old_state(new)
  })

}

update_table_val_js = function(selector, val) {
  paste0(
    "
    function update_table($sel, val) {
      if ($sel) {
        $sel.text(val);
      } else {
       setTimeout(update_table($sel, val), 100);
      }
    }
    ",
    glue::glue("update_table($('{selector}'), {val});")
  )
    # glue::glue("$('{selector}').text('{val}');")
}

build_table = function(data, id = '') {
  shiny::div(
    id = id,
    shiny::tags$table(
      shiny::tags$thead(
        shiny::tags$tr(
          purrr::map(colnames(data), ~shiny::tags$th(style = "text-align:right;", .x))
        )
      ),
      shiny::tags$tbody(
        purrr::map(seq_len(nrow(data)), function(x) {
          shiny::tags$tr(
            purrr::map(data[x,], function(y) {
              shiny::tags$td(
                style = "alight: right;",
                y
              )
            })
          )
        })
      )
    )
  )
}

colour_flash_js = function(selector, colour = "green") {
  paste0(
    glue::glue("$('{selector}')"),
    '.css({
      "background-color": "', colour,'",
      "transition-duration": "0s",
      "transition-timing-function": "linear",
      "-webkit-transition-duration": "0s",
      "-webkit-transition-timing-function": "linear",
      "transition-property": "background-color",
      "-webkit-transition-property": "background-color",
    });

    ',
    "setTimeout(() => {",
    glue::glue("$('{selector}')"),
    '.css({
      "background-color": "white",
      "transition-duration": "1s",
      "transition-timing-function": "linear",
      "-webkit-transition-duration": "1s",
      "-webkit-transition-timing-function": "linear",
      "transition-property": "background-color",
      "-webkit-transition-property": "background-color",
    })

    ',
    "},100);")
}

# shiny::shinyApp(ui, server)
