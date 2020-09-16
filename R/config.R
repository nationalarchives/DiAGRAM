#' Read config
#'
#' Reads the YAML question configuration from file on disk
#' @param file File path to read from
#' @importFrom yaml yaml.load_file
#' @export
read_config = function(file) {
  yaml::yaml.load_file(
    file,
    handlers = list(
      'bool#yes' = function(x) if(tolower(x) %in% c('yes','y')) x else TRUE,
      'bool#no' = function(x) if(tolower(x) %in% c('no','n')) x else FALSE
    )
  )
}

write_config = function(data) {

}
