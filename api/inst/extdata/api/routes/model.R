requireNamespace("gRain")

# endpoint function closure, only load model once
endpoint = function() {
  model = diagramAPI::load_default_model()
  function(req) {
    diagramAPI::score_model(req, model)
  }
}

#' @apiTitle Score a model
#' @apiDescription Score a model or scenario based on it's inputs by executing
#' the underlying bayesian network model.
#' @post /score
#' @serializer unboxedJSON
endpoint()
