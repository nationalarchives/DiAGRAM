# model = bnlearn::read.bif("inst/default_model/Model.bif")
# default_response = load_single_response("inst/default_model/default_response.json")
# questions = question_data = read_config("temp.yaml")
# responses = default_response
project_scores = function(responses, question_data) {
  types = question_data %>% purrr::map_chr('type')
  # nam
  # for the single sliders, we could increase each to by 10 and 25%?

}



# x = jsonlite::read_json("inst/default_model/commercial_backup.json")
