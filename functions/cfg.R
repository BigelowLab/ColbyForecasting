
fetch_observations = function(cfg, verbose = TRUE){
  #' Fetch observation data - stores in species/data
  #' 
  #' @param cfg configuration list
  #' @param verbose logical, if TRUE then output helpful messages
  #' @return configuration list
  
  x = fetch_obis(scientificname = cfg$scientificname,
                 bb = cfg$bbox |> bbox_to_polygon(),
                 fields = standard_obis_fields(),
                 tidy = TRUE,
                 output_path = data_path("obis") |> make_path(),
                 crs = 4326)
  
  if (verbose){
    cat(sprintf("Downloaded %i records for %s in '/data/obis/%s.gpkg'",
                nrows(x), cfg$species, scientific_name(cfg$scientificname)), "\n")
  }
  
  cfg
}

cfg_template = function(name = "mola",
                        scientificname = "Mola mola",
                        version = "v1.00",
                        bbox = colby_bbox(form = "vector")){
  
  #' Create a template configuration
  #' 
  #' @param name chr, the short easy name of the species (no spaces, weird characters or caps)
  #' @param scientificname chr, the formal display name for plots and reports
  #' @param bbox num, 4 element bounding box (xmin, ymin, xmax, ymax)
  #' @param version chr, version identifier
  #' @return configuration list
  
  list(name = name[1],
       scientificname = scientificname[1],
       version = version[1],
       bbox = bbox,
       point_selection_fun = "basic_obs_and_background_selection")
}

