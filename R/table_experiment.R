# # model
# model = bnlearn::read.bif(system.file("default_model/Model.bif",package = "diagramNAT"))
#
# # original response
# default = load_responses("inst/default_model/default_response.json")
#
# model_policy_row = function(responses, model_name, policy_name = NA, notes = NA) {
#   tibble::tibble(model = model_name, policy = policy_name, notes = notes ,response = list(responses))
# }
#
# # model 1 policy 1back
# p1 = default
# p1[[4]] = 70
#
#
#
# # policy 2
# p2 = p1
# p2[[1]] = 40
#
# model1 = dplyr::bind_rows(
#   default %>% model_policy_row("default"),
#   p1 %>% model_policy_row("default", "policy 1", "If we could increase info management to 70%"),
#   p2 %>% model_policy_row("default", "policy 2", "If we could increase info management to 70% and technical skills to 40%")
# )
#
# # new model
# m2 = generate_example_reponse()
#
# # model 2 policy 1
# m2p1 = m2
# m2p1[[6]] = 1:3*100/6
#
# model2 = bind_rows(
#   m2 %>% model_policy_row("random model", notes = "Just set a model with random values"),
#   m2p1 %>% model_policy_row("random model", "random policy", "same random model, with some values randomly tweaked")
# )
#
# temp = bind_rows(
#   model1,
#   model2
# )
#
# temp = bind_cols(temp, purrr::map_dfr(df$response, ~{
#   score_model(model, format_responses(.x)) %>% unlist
# }))
#
#
#
# # mutate(response = purrr::map_chr(response, ~{.x %>% jsonlite::toJSON(auto_unbox = TRUE) %>% as.character()})) %>%
#
# temp %>%
#   select(model, policy, "Intellectual Control" = Intellectual_Control, Renderability, notes) %>%
#   mutate_if(is.numeric, ~round(.x,2)) %>%
#   reactable::reactable(
#     groupBy = "model",
#     details = function(index) {
#       print(index)
#       res = temp[index,]$response[[1]]
#       tbl = reactable::reactable(format_responses(res))
#       htmltools::div(style = list(margin = "12px 45px"), tbl)
#     },
#     # onClick = "expand",
#     rowStyle = list(cursor = "pointer")
#   )
