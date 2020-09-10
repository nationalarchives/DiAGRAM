load_config = function(file) {
  yaml::yaml.load_file(
    file,
    handlers = list('bool#yes' = function(x) if(x %in% c('Yes','yes')) x else TRUE)
  )
}
