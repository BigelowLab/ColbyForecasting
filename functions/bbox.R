colby_bbox = function(x = c(xmin = -74.9, 
                            xmax = -65, 
                            ymin = 38.8, 
                            ymax = 46),
                      form = c("polygon", "bbox", "vector")[1]){
  
  #' Retrieve the region of study coordinates
  #' 
  #' @param x num, named vector defining the bounding box.  We provide
  #'   a default but you can override these values.
  #' @param form chr, names the output form of data as "vector",
  #'   "bbox" (from sf package), or "polygon" (from sf package)
  #' @return bbox or polygon sf object
  
  r = switch(tolower(form[1]),
    "bbox" = sf::st_bbox(x, crs = 4326),
    "polygon" = sf::st_bbox(x, crs = 4326) |> sf::st_as_sfc() |> sf::st_as_sf(),
    x)
  r
}
