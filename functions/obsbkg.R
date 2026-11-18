extract_obsbkg_env = function(cfg,
                              x = read_obsbkg(cfg)){
  
  #' Select observations and background points, by thinning and weighted sampling.
  #' Points are save in "<scientificname>/<version>/obsbkg_<something>.gpkg" where
  #' <something> indicates the scenario, period and interval (of which we only deal
  #' with month)
  #' 
  #' @param cfg configuration list
  #' @param x data frame of obsbkg point locations
  #' @return the input configuration list

  path = data_path("brickman")
  opath = data_path(scientific_name(cfg$scientificname),
                    cfg$version,
                    "data") |>
    make_path()
  r = env_database(path) |>
    dplyr::filter(interval %in% c("mon")) |>
    dplyr::mutate(.name = paste(.data$scenario, .data$year, .data$interval, sep = "-"))
  static = env_database(path) |>
    dplyr::filter(scenario == "STATIC") |>
    read_env(path)
  geomcol = attributes(x)$sf_column
  r = r |>
    dplyr::group_by(scenario, year, interval) |>
    dplyr::group_map(
      function(grp, key){
        ofile = file.path(opath, 
                          sprintf("obsbkg_%s.gpkg", grp$.name[1]))
        stat = stars::st_extract(static, x |> sf::st_coordinates())
        s = read_env(grp, path)
        extract_env_by_month(s, x, form = "wide") |>
          dplyr::bind_cols(stat) |>
          dplyr::relocate(dplyr::all_of(geomcol), 
                          .after = dplyr::last_col()) |>
          dplyr::select(-dplyr::any_of(".id")) |>
          sf::write_sf(ofile)  
      }, .keep = TRUE)
  
  cfg
}



select_obs_and_background = function(cfg,
                             obsbkg_function = cfg$point_selection_fun,
                             extra_name = NULL,
                             ...){
 
  #' Select observations and background points, by thinning and weighted sampling.
  #' Points are save in "<scientificname>/<version>/obsbkg.gpkg" unless the caller
  #' provides `extra_name` in which case the file is saved as 
  #' "<scientificname>/<version>/obsbkg_<extra_name>.gpkg". Also, a plot of monthly distributions is
  #' saved in "<scientificname>/<version>/images/obsbkg_month.png"
  #' 
  #' @param cfg configuration list
  #' @param obsbkg_function the name of the function to select obs and background points,
  #'   This is likely in the config, but you can override it here
  #' @param extra_name chr, in case you want to save this run with an extra added to the
  #'   output filename
  #' @param ... other arguments for `obsbkg_function`
  #' @return the input configuration list
  fun = get(obsbkg_function)
  
  obsbkg = fun(cfg, extra_name = extra_name, ...)

  gg = map_obsbkg(cfg, x = obsbkg, extra_name = NULL, save_file = TRUE)
  
  cfg  
}

basic_obs_and_background_selection = function(cfg,
                                              extra_name = NULL,
                                              opath = data_path(scientific_name(cfg$scientificname), 
                                                                cfg$version, 
                                                                "data"),
                                              ...){
  #' A basic function for selecting observations and background by thinning and
  #' weighted sampling.  You can write your own if you have different needs or ideas.
  #' @param extra_name chr, in case you want to save this run with an extra added to the
  #'   output filename
  #' @param opath chr, the output path
  #' @param ... unused for now
  #' @return data frame (spatial) of observations and background points
  
  opath = make_path(opath)
  obs = read_observations(cfg)
  mask = read_mask()
  n_per_month = dplyr::count(sf::st_drop_geometry(obs), month) |> 
    dplyr::pull(n) |>
    mean(na.rm = TRUE)
  
  thinned_obs = sapply(month.abb,
                       function(mon){ 
                         tidysdm::thin_by_cell(obs |> dplyr::filter(month == mon), mask)
                       }, simplify = FALSE) |>
    dplyr::bind_rows() 
  
  map_observations(cfg, x = thinned_obs, interval = "month", extra_name = "thinned_obs")
  
  n_thinned_per_month = dplyr::count(sf::st_drop_geometry(thinned_obs), month) |> 
    dplyr::pull(n) |>
    mean(na.rm = TRUE)
  
  bias_map = rasterize_point_density(obs, mask)
 
  
  obsbkg = sapply(month.abb,
                  function(mon){ 
                    sample_background(thinned_obs |> filter(month == mon), # <- just this month
                                      bias_map,
                                      method = "bias",  # <-- it needs to know it's a bias map
                                      return_pres = TRUE, # <-- give me the obs back, too
                                      n = n_thinned_per_month) |>   # <-- how many points
                      mutate(month = mon, .before = 1)
                  }, simplify = FALSE) |>
    bind_rows() |>
    mutate(month = factor(month, levels = month.abb)) |>
    sf::write_sf(file.path(opath, 
                           ifelse(is_nullna(extra_name),
                                  "obsbkg.gpkg",
                                  sprintf("obsbkg_%s.gpkg", extra_name[1]))))
  obsbkg
}


read_obsbkg_env = function(cfg, 
                           name = "PRESENT-PRESENT-mon",
                           form = c("wide", "long")[1]){
  
  #' Read a data frame (spatial) of observation and background env data for a given
  #' scenario-year-interval combiantion
  #' 
  #' @param cfg configuration list
  #' @param name chr, the name of the scenario-year-interval combo
  #' @param form chr, either "wide" (default) or "long"
  #' @return data frame (spatial)
  
  x = read_obsbkg(cfg, extra_name = name)
  if (tolower(form[1]) == "long"){
    geomcol = attributes(x)$sf_column
    x = tidyr::pivot_longer(x,
                            -dplyr::any_of(c("month", "class",geomcol)),
                            names_to = "var",
                            values_to = "value") |>
      dplyr::relocate(dplyr::all_of(geomcol), 
                      .after = dplyr::last_col())
  }
  x
}

read_obsbkg = function(cfg, extra_name = NULL){
  
  #' Read a data frame (spatial) of observation and background points
  #' 
  #' @param cfg configuration list
  #' @param extra_name chr, and optional extra name added to the file that you want
  #'   to read.  See [select_obs_and_background]
  #' @return data frame (spatial)
  file = data_path(scientific_name(cfg$scientificname), 
                   cfg$version, 
                   "data",
                   ifelse(is_nullna(extra_name),
                          "obsbkg.gpkg",
                          sprintf("obsbkg_%s.gpkg", extra_name[1])))
  sf::read_sf(file)
}

map_obsbkg = function(cfg,
                       x = read_obsbkg(cfg),
                       save_file = TRUE,
                       extra_name = NULL){
  #' Map observations and background or month 
  #' 
  #' @param cfg configuration list
  #' @param x data frame of the data to map
  #' @param save_file logical, if TRUE then save the file disk in 
  #'    <species>/<version>/map_<interval>.png
  #' @param extra_name NA (or NULL) of character, something to add to the filename, by default
  #'   nothing is added, but use this to add, say, "thinned" or whatever
  #' @return, ggplot2 object
  #' 
  coast = read_coastline()
  gg = ggplot2::ggplot() +
    ggplot2::geom_sf(data = obsbkg, 
            mapping = ggplot2::aes(col = class),
            alpha =  0.4, shape = "circle small", size = 1) +
    ggokabeito::scale_fill_okabe_ito() +  # <-- colorblind friendly for N Record
    ggplot2::geom_sf(data = coast, col = "orange")  + 
    ggplot2::labs(x = NULL, y = NULL, 
                  title = sprintf("%s %s", cfg$scientificname, cfg$version)) +   
    ggplot2::theme_bw() +  # <- make a simple white background 
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle=90, vjust=.5, hjust=1)) + 
    ggplot2::facet_wrap(~month)
  
  if(save_file){
    path =  data_path(scientific_name(cfg$scientificname), cfg$version, "images") |>
      make_path()
    ofile = file.path(path, ifelse(is_nullna(extra_name[1]), 
                                   "map_obsbkg_month.png",
                                   sprintf("map_obsbkg_%s.png", extra_name[1])))
    ggplot2::ggsave(ofile, plot = gg, width = 8, height = 8)
  }
  gg
}
