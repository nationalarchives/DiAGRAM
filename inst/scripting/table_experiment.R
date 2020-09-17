# model
model = bnlearn::read.bif(system.file("default_model/Model.bif",
                                      package = "diagramNAT"))

# original response
default = diagramNAT:::load_single_response("inst/default_model/default_response.json")

model_policy_row = function(responses, model_name, policy_name = NA, notes = NA) {
  tibble::tibble(model = model_name, policy = policy_name, notes = notes ,response = list(responses))
}

# model 1 policy 1back
p1 = default
p1[[4]] = 70



# policy 2
p2 = p1
p2[[1]] = 40

model1 = dplyr::bind_rows(
  default %>% model_policy_row("default"),
  p1 %>% model_policy_row("default", "policy 1", "If we could increase info management to 70%"),
  p2 %>% model_policy_row("default", "policy 2", "If we could increase info management to 70% and technical skills to 40%")
)

# new model
library("diagramNAT")
m2 =diagramNAT:::generate_example_reponse()

# model 2 policy 1
m2p1 = m2
m2p1[[6]] = 1:3*100/6

model2 = dplyr::bind_rows(
  m2 %>% model_policy_row("random model", notes = "Just set a model with random values"),
  m2p1 %>% model_policy_row("random model", "random policy", "same random model, with some values randomly tweaked")
)

temp = dplyr::bind_rows(
  model1,
  model2
)

temp = dplyr::bind_cols(temp, purrr::map_dfr(temp$response, ~{
  diagramNAT:::score_model(model, diagramNAT:::format_responses(.x)) %>% unlist
}))


# mutate(response = purrr::map_chr(response, ~{.x %>% jsonlite::toJSON(auto_unbox = TRUE) %>% as.character()})) %>%
library("htmltools")

bar_chart = function(label,
                     width = "100%",
                     height = "16px",
                     fill = "#00bfc4",
                     background = NULL) {
  bar = div(style = list(
    background = fill,
    width = width,
    height = height
  ))
  chart = div(style = list(
    flexGrow = 1,
    marginLeft = "8px",
    background = background
  ),
  bar)
  div(style = list(display = "flex", alignItems = "center"), label, chart)
}

temp %>%
  reactable::reactable(
    groupBy = "Model",
    columns = list(
      `Intellectual Control` = reactable::colDef(
        align = "left",
        cell = function(value) {
          width = paste0(value / 1 * 100, "%")
          bar_chart(value, width = width, fill = "#358694")
        }
      ),
      Renderability = reactable::colDef(
        align = "left",
        cell = function(value) {
          width = paste0(value / 1 * 100, "%")
          bar_chart(value, width = width, fill = "#354694")
        }
      )
    ),
    details = function(index) {
      print(index)
      res = temp[index, ]$response[[1]]
      tbl = reactable::reactable(format_responses(res))
      htmltools::div(style = list(margin = "12px 45px"), tbl)
    },
    # onClick = "expand",
    #   rowStyle = list(cursor = "pointer"),
    borderless = TRUE,
    onClick = "select",
    selection = "single",
    highlight = TRUE,
    rowStyle = htmlwidgets::JS(
      "function(rowInfo) {
    if (rowInfo && rowInfo.selected) {
      return { backgroundColor: '#eee', boxShadow: 'inset 2px 0 0 0 #358694' }
    }
  }"
    ),
    style = list(fontFamily = "lato"),
  )

plot = sparkline::sparkline(values = c(1, 0.8), type = "bullet", chartRangeMin = 0, chartRangeMax = 1)
plot$jsHooks
