questions = diagramNAT::read_config(system.file("text_content", "questions_auto_gen.yml", package = "diagramNAT"))
## for now, drop the last question
questions = questions[-length(questions)]
default_response = diagramNAT::load_single_response(system.file("default_model", "commercial_backup.json", package = "diagramNAT"))
model = bnlearn::read.bif(system.file("default_model", "Model.bif", package = "diagramNAT"))

diagramNAT::run_app(questions, default_response, model)

