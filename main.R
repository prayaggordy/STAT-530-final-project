config <- yaml::read_yaml("config.yaml")
purrr::walk(list.files(path = "R", pattern = "*.R", full.names = T), source)

