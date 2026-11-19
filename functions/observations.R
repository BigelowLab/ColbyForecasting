histogram_observations = function(cfg,
                                  x = read_observations(cfg), 
                                  interval = c("year","month")[1],
                                  save_file = TRUE,
                                  log_scale = FALSE,
                                  year_limits = c(1982, 2013),
                                  extra_name = NULL){
  #' Plot observations histogramed by year or month
  #' by year or month
  #' 
  #' @param cfg configuration list
  #' @param x data frame of observations
  #' @param interval chr, the interval to bin
  #' @param save_file logical, if TRUE then save the file disk in 
  #'   <species>/<version>/histogram_<interval>.png
  #' @param log_scale logical, if TRUE log the y axis
  #' @param year_limits numeric or NULL, when plotting "year" add
  #'   vertical dashed lines at these times.  To skip set to NULL. Only
  #'   applies when `interval` is "year".
  #' @param extra_name NA (or NULL) of character, something to add to the filename, by default
  #'   nothing is added, but use this to add, say, "thinned"
  #' @return, ggplot2 object
  

  # we don't need the spatial info
  x = sf::st_drop_geometry(x) 
  
  gg = ggplot2::ggplot(data = x,
                  mapping = ggplot2::aes(x = .data[[interval]])) 
  if (interval == "month"){
    gg = gg + 
      ggplot2::geom_bar()
  } else {
    gg = gg + 
      ggplot2::geom_histogram(binwidth = 1) 
    if (!is.null(year_limits)){
      gg = gg + 
        ggplot2::geom_vline(xintercept = as.numeric(year_limits), 
                            linetype = "dashed")
    } # year_limits?
  } # year or month?
  
  gg = gg +   
    ggplot2::labs(title = sprintf("%s %s", cfg$scientificname, cfg$version))
    
  if (log_scale) {
    #https://stackoverflow.com/questions/41849951/using-ggplot-geom-histogram-with-y-log-scale-with-zero-bins
    # scale_y_continuous(trans = scales::log1p_trans()
    
    gg = gg + 
      ggplot2::scale_y_continuous(trans = scales::transform_pseudo_log(base = 10))
  }
  
  if (save_file){
    path =  data_path(scientific_name(cfg$scientificname), cfg$version, "images") |>
      make_path()
    ofile = file.path(path, ifelse(is_nullna(extra_name[1]), 
                                   sprintf("histogram_%s.png", interval),
                                   sprintf("histogram_%s_%s.png", interval, extra_name[1])))
    ggplot2::ggsave(ofile, plot = gg, width = 8, height = 6)
  }
  gg
}

map_observations = function(cfg,
                            x = read_observations(cfg),
                            interval = c("all", "month", "year")[1],
                            include_depth = TRUE,
                            save_file = TRUE, 
                            extra_name = NULL){
  #' Map observations by year or month or just lumped together
  #' 
  #' @param cfg configuration list
  #' @param x data frame of the data to map
  #' @param interval chr, the interval to bin, all to just plot all regardless 
  #'   of the interval
  #' @param save_file logical, if TRUE then save the file disk in 
  #'    <species>/<version>/map_<interval>.png
  #' @param extra_name NA (or NULL) of character, something to add to the filename, by default
  #'   nothing is added, but use this to add, say, "thinned"
  #' @return, ggplot2 object
  
  coast = read_coastline()
  depth = read_depth(contours = TRUE)
  
  gg = ggplot2::ggplot(data = x)
  
  # add depth?
  if (include_depth){
    gg = gg + 
      ggplot2::geom_sf(data = depth, 
                       color = "gray50")
  }
   
  # add the coast
  gg = gg + 
    ggplot2::geom_sf(data = coast, color = "orange") 

  
  int = tolower(interval[1])
  if(int == "month"){
    LON0 = -67
    LAT0 = 46
    all_counts = dplyr::count(sf::st_drop_geometry(x), .data$month)
    
    gg = gg + 
      ggplot2::geom_sf(size = 0.3, alpha = 0.1, color = "blue") + 
      ggplot2::geom_text(data = all_counts,
                         mapping = ggplot2::aes(x = LON0, 
                                                y = LAT0, 
                                                label = sprintf("n: %i", .data$n)),
                         size = 3) + 
      ggplot2::facet_wrap(~month)
  } else if (int == "year"){
    gg = gg +
      ggplot2::geom_sf(mapping = ggplot2::aes(color = year),
                       size = 0.3, alpha = 0.1) + 
      ggplot2::scale_color_distiller(palette = "Purples", 
                                    type = "seq", 
                                    direction = 1)
  }  else {
    gg = gg +
      ggplot2::geom_sf(size = 0.3, alpha = 0.1, color = "blue")
  }
  
  gg = gg + 
    ggplot2::labs(x = NULL, 
                  y = NULL,
                  title = sprintf("%s %s", cfg$scientificname, cfg$version)) + 
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle=90, vjust=.5, hjust=1))
  
  if (save_file){
    path =  data_path(scientific_name(cfg$scientificname), cfg$version, "images") |>
      make_path()
    ofile = file.path(path, ifelse(is_nullna(extra_name[1]), 
                                   sprintf("map_%s.png", interval),
                                   sprintf("map_%s_%s.png", interval, extra_name[1])))
    ggplot2::ggsave(ofile, plot = gg, width = 8, height = 6)
  }
  gg
}

read_observations = function(cfg,
                             filter_fun = NULL, 
                             ...){
  
  #' Read raw OBIS data and then possibly filter it
  #' 
  #' @param cfg configuration list
  #' @param filter_fun chr, function name or NULL, to perform basic data filtering,
  #'   set to NULL to skip.  If `cfg` has a obs_filter_fun element then that will be used.
  #' @param ... other arguments passed to `read_obis()`
  #' @return a filtered table of observations
  
  # read in the raw data
  x = read_obis(cfg$scientificname, ...) |>
    dplyr::mutate(month = factor(month, levels = month.abb))
  
  # if the user provided a non-NULL filter function
  if (is.null(filter_fun)){
    if(!is.null(cfg$obs_filter_fun)){
      filter_fun = cfg$obs_filter_fun
    }
  }
  if (!is.null(filter_fun)){
    fun = get(filter_fun)   # throws an error if not found
    x = x |>
      fun()
  }
  
  return(x)
}


mola_filter = function(x,
                        minimum_year = 1970){
  #' Provide basic filtering for a observation dataset for Mola mola
  #' 
  #' You can use this as a template for your own filtering function.
  #' 
  #' @param x data frame (possibly spatial), table of observations
  #' @param minimum_year num, threshold for year
  x = dplyr::filter(x, !is.na(.data$eventDate), 
                       .data$year >= minimum_year[1])
  return(x)
}

browse_observation = function(id = "00040fa1-7acd-4731-bf1e-6dc16e30c7d4",
                       base_url = "https://api.obis.org/v3/occurrence"){
  
  #' Browse the record for one unique id
  #' 
  #' @param id chr, a unique ID or an OBIS record (one row) If multiples are 
  #'   provided all but the first are ignored.
  #' @param root_url chr, the root url
  #' @return NULL invisibly
  #' @examples
  #' \dontrun{
  #'   obs = read_observations(cfg)
  #'   one = obs |> slice(202)
  #'   browse_obis(one)
  #' }
  
  if (inherits(id, "data.frame")) id = dplyr::pull(id, id)
  httr::BROWSE(file.path(base_url, id[1]))
}
