library("diagramLambda")

# Ephemeral storage
tmp_dir = "/tmp"

# https://github.com/rstudio/rmarkdown/issues/1975
options(
  tinytex.output_dir = tmp_dir,
  tinytex.engine_args = glue::glue("'--output-directory={tmp_dir}'")
)

logger::log_formatter(logger::formatter_paste)
logger::log_threshold(logger::DEBUG)

handler = function(event_content, context) {
  diagramLambda::handler(event_content, context)
}

lambdr::start_lambda(
  config = lambdr::lambda_config(
    serialiser = identity,
    deserialiser = identity
  )
)
