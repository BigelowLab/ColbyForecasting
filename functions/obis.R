

standard_obis_fields = function(){
  #' Retrieve a vector of standard OBIS fields
  #' 
  #' @return character vector of names
  fields = c("id",
             "basisOfRecord",
             "eventDate",
             "eventTime",
             "individualCount",
             "absence",
             "decimalLongitude",
             "decimalLatitude")
  return(fields)
}

tidy_obis = function(x = fetch_obis(),
                    fields = standard_obis_fields(),
                    crs = 4326){
  
  #' Tidy raw OBIS results and return as an sf object
  #' 
  #' @param x data frame or tibble of raw obis results
  #' @param fields chr, the preferred fields and order of fields
  #' @param crs the CRS of the data
  #' @return sf object
  
  x = dplyr::select(x, dplyr::any_of(fields))
  cnames = colnames(x)
  if ("eventDate" %in% cnames) {
    dates = substring(x$eventDate, 1,10) |> as.Date()
    x = dplyr::mutate(x, 
                      eventDate = dates,
                      year = suppressWarnings(as.numeric(format(eventDate, "%Y"))),
                      month = suppressWarnings(format(eventDate, "%b")) |>
                        factor(levels = month.abb),
                      .after = eventDate)
  }
  if ("individualCount" %in% cnames) {
    x = dplyr::mutate(x,individualCount = suppressWarnings(as.numeric(individualCount)))
  }
  
  sf::st_as_sf(x, 
               coords = c("decimalLongitude", "decimalLatitude"),
               crs = crs) 
}



fetch_obis = function(scientificname = "Mola mola",
                      bb = colby_bbox(),
                      fields = standard_obis_fields(),
                      tidy = TRUE,
                      output_path = data_path("obis") |> make_path(),
                      crs = 4326){
  
  #' Fetch one or more species of obis data
  #' 
  #' @param scientificname one or more "genus" or "genus species" scientific name(s)
  #' @param bb either NULL (to skip) or something from which a bounding box may be
  #'   extracted.  Used to filter the request to a smaller-than-global scope.
  #'   The output oc `colby_bbox()` will do nicely.
  #' @param fields chr one or more fields to retrieve
  #' @param tidy logical, if TRUE tidy up the downloaded data
  #' @param output_path chr, the destination path where the data are stored using 
  #'  Files are automatically named "genus_species.gpkg" or "genus.gpkg" as
  #'  appropriate for each request.
  #' @param crs the CRS for the data
  #' @return spatial table of observations
  #'  
  geometry = if(is.null(bb)){
    NULL
  } else {
    if (inherits(bb, "bbox")) bb = sf::st_as_sfc(bb)
    if (!inherits(bb, "sfc")) bb = sf::st_as_sfc(bb)
    sf::st_as_text(bb)
  }
  
  xx = lapply(scientificname,
              function(species){
                x = if ("absence" %in% fields){
                    robis::occurrence(scientificname = species,
                                      geometry = geometry,
                                      fields = fields,
                                      absence = "include")
                } else {
                  robis::occurrence(scientificname = species,
                                    geometry = geometry,
                                    fields = fields)
                } 
                  
                if (tidy) x = tidy_obis(x, fields = fields, crs = crs)
                sf::write_sf(x, 
                             file.path(output_path, 
                                       sprintf("%s.gpkg", gsub(" ", "_", species, fixed = TRUE))))
              }) |>
    dplyr::bind_rows()
  return(xx)
}



read_obis = function(scientificname = "Mola mola",
                     path = data_path("obis")){
  
  #' Read one or more obis data files
  #' 
  #' @param scientificname one or more scientific names to read
  #' @param path chr the data path
  #' @return sf table
  
  xx = lapply(scientificname,
              function(species){
                filename = file.path(path, 
                           sprintf("%s.gpkg", gsub(" ", "_", species, fixed = TRUE)))
                sf::read_sf(filename)
              }) |>
    dplyr::bind_rows()
  return(xx)
}




browse_obis = function(id = "00040fa1-7acd-4731-bf1e-6dc16e30c7d4",
                       base_url = "https://api.obis.org/v3/occurrence"){
  
  #' Browse the record for one unique id
  #' 
  #' @param id chr, a unique ID or an OBIS record (one row) If multiples are 
  #'   provided all but the first are ignored.
  #' @param root_url chr, the root url
  #' @return NULL invisibly
  #' @examples
  #' \dontrun{
  #'   obs = read_obis(scientificname = "Mola mola")
  #'   one = obis |> slice(202)
  #'   browse_obis(one)
  #' }
  
  if (inherits(id, "data.frame")) id = dplyr::pull(id, id)
  httr::BROWSE(file.path(base_url, id[1]))
}