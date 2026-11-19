fetch_taxon = function(cfg){
  #' Fetch taxon metadata
  #' 
  #' @param cfg configuration list
  #' @return the input
  x = taxon_obis(cfg$scientificname) |>
    yaml::write_yaml(data_path("obis", 
                               sprintf("%s-taxon.yaml",
                                       scientific_name(cfg$scientificname))))
  cfg
}

read_taxon = function(cfg){
  #' Read the taxon metadata
  #' 
  #' @param cfg configuration list
  #' @return list of taxon metadata
  
  filename = data_path("obis", 
                       sprintf("%s-taxon.yaml",
                               scientific_name(cfg$scientificname)))
  if (!file.exists(filename)){
    cfg = fetch_taxon(cfg)
  }
  
  yaml::read_yaml(filename)
}

browse_taxon = function(cfg,
                        base_url = "https://api.obis.org/v3/taxon"){
  
  #' Browse the taxon record for one species
  #' 
  #' @param cfg configuration list
  #' @param root_url chr, the root url
  #' @return NULL invisibly
  httr::BROWSE(file.path(base_url, cfg$scientificname))
  invisible(NULL)
}

taxon_obis = function(scientificname = "Mola mola",
                      base_url = "https://api.obis.org/v3/taxon"){
  #' Download taxon information given a scientific nane
  #' 
  #' @param scientificname chr, the scientific name
  #' @param base_url chr, the base OBIS url
  #' @return a list of metadata
  
  # https://api.obis.org/v3/taxon/Mola%20mola
  uri = file.path(base_url, scientific_name(scientificname[1], sep = "%20"))
  x = httr::GET(uri)
  httr::stop_for_status(x, task = "download")
  httr::content(x)$results[[1]]
}
