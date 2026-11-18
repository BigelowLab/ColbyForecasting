
read_configuration = function(scientificname = "Mola mola",
                              version = "v1.00",
                              path = data_path(scientific_name(scientificname), version)){
  
  #' Given a version identifier, read a configuration
  #' 
  #' @param scientificname str species name
  #' @param version chr the version identifier (defaults to "v1.00")
  #' @param path chr you personal model data path (defaults to `data_path(scientific_name(scientificname))`)
  #' @return configuration list
  
  filename = file.path(path, "config.yaml")
  yaml::read_yaml(filename)
}



write_configuration = function(cfg){
  
  #' Given a versioned configuration write a configuration
  #' 
  #' @param cfg list, must have element named "version"
  #' @return the input configuration list
  
  
  if (!"version" %in% names(cfg)) stop("input config must have version element")
  
  path = data_path(scientific_name(cfg$scientificname), cfg$version) |> make_path()
  
  filename = file.path(path, "config.yaml")
  ok = yaml::write_yaml(cfg, filename)
  cfg
}
