#' Read config
#'
#' Reads the YAML question configuration from file on disk
#' @param file File path to read from
#' @importFrom yaml yaml.load_file
#' @export
read_config = function(file) {
  yaml::yaml.load_file(
    file,
    handlers = list('bool#yes' = function(x) if(x %in% c('Yes','yes')) x else TRUE)
  )
}

write_config = function(data) {

}
