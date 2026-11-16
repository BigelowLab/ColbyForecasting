
read_configuration = function(scientificname = "Mola mola",
                              version = "v1.00",
                              path = data_path(scientific_name(scientificname))){
  
  #' Given a version identifier, read a configuration
  #' 
  #' @param scientificname str species name
  #' @param version chr the version identifier (defaults to "v1.00")
  #' @param path chr you personal model data path (defaults to `data_path(scientific_name(scientificname))`)
  #' @return configuration list
  
  filename = file.path(path, sprintf("%s-%s.yaml", 
                                     scientific_name(scientificname),
                                     version[1]))
  yaml::read_yaml(filename)
}



write_configuration = function(cfg = cfg_template()){
  
  #' Given a versioned configuration write a configuration
  #' 
  #' @param cfg list, must have element named "version"
  #' @return the input configuration list
  
  path = data_path(scientific_name(cfg$scientificname)) |> make_path()
  
  if (!"version" %in% names(cfg)) stop("input config must have version element")
  
  filename = file.path(path, sprintf("%s-%s.yaml", 
                                     scientific_name(cfg$scientificname),
                                     cfg$version[1]))
  yaml::write_yaml(cfg, filename)
}
