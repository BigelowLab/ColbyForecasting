obsbkg_extract_env = function(cfg,
                       x = read_obsbkg(cfg),
                       path = data_path("brickman"),
                       opath = data_path("data")){
  opath = make_path(opath)
  r = env_database(path) |>
    dplyr::mutate(.name = paste(.data$scenario, .data$year, .data$interval, sep = "-"))
  static = r |>
    dplyr::filter(scenario == "STATIC") |>
    read_env(path)
  geomcol = attributes(x)$sf_column
  r = r |>
    dplyr::filter(interval %in% c("mon")) |>
    dplyr::group_by(scenario, year, interval) |>
    dplyr::group_map(
      function(grp, key){
        ofile = file.path(opath, 
                          sprintf("obsbkg_%s.gpkg", grp$.name[1]))
        stat = stars::st_extract(static, x |> sf::st_coordinates())
        s = read_env(grp, path)
        v = extract_env_by_month(s, x, form = "wide") |>
          dplyr::bind_cols(stat) |>
          dplyr::relocate(dplyr::all_of(geomcol), 
                          .after = dplyr::last_col())
      }, .keep = TRUE)
}