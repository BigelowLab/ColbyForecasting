#' Fetch observation data
#' 
#' @param cfg configuration list
#' @return configuration list
fetch_observations = function (cfg){
  
}


#' Create a template configuration
#' 
#' @param species chr, the short easy name of the species (no spaces, weird characters or caps)
#' @param scientificname chr, the formal display name for plots and reports
#' @param bbox num, 4 element bounding box (xmin, ymin, xmax, ymax)
#' @param version chr, version identifier
#' @return configuration list
cfg_template = function(species = "dog",
                        scientificname = "Canis familiaris",
                        version = "v1.00",
                        bbox = colby_bbox(form = "vector")){
  list(species = species[1],
       scientificname = scientificname[1],
       version = version[1],
       bbox = bbox)
}

