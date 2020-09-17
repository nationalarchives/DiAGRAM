questions = diagramNAT::read_config(system.file("text_content", "final_questions.yml", package = "diagramNAT"))
## for now, drop the last question
# questions = questions[-length(questions)]
default_response = diagramNAT::load_single_response(system.file("default_model", "new.json", package = "diagramNAT"))
# default_response = default_response[-length(default_response)]
model = bnlearn::read.bif(system.file("default_model", "Model.bif", package = "diagramNAT"))

# default_response$Physical_Disaster = "Very Low"

diagramNAT::run_app(questions, default_response, model)

