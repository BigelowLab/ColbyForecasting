
brickman_variables = function(interval = c("mon", "ann", "all")[1]){
  
  #' Retrieve a listing of all Brickman variables
  #' 
  #' @param interval chr, one of "mon" (monthly), "ann" (annual) or "all"
  #' @return table
  
  x = dplyr::tribble(
    ~name, ~group, ~longname, ~description, ~units,
    'depth', "static", "Bathy_depth", "bathymetric depth", "m",
    'Month', "mon", "month", "month of the year", "",
    'mask',  "static", "land_mask", "land mask", "",
    'SST', "ann", "SST_ann", "annual SST", "C",
    'SST', "mon", "SST", "monthly SST", "C",
    'MLD', "ann", "MLD_ann", "annual mixed layer depth", "m",
    'MLD', "mon", "MLD", "monthly mixed layer depth", "m",
    'SSS', "ann", "SSS_ann", "annual SSS", "psu",
    'SSS', "mon", "SSS", "monthly SSS", "psu",
    'Tbtm', "ann", "Tbtm_ann", "annual Tbtm", "C",
    'Tbtm', "mon", "Tbtm", "monthly Tbtm", "C",
    'Sbtm', "ann", "Sbtm_ann", "annual Sbtm", "psu",
    'Sbtm', "mon", "Sbtm", "monthly Sbtm", "psu",
    'Xbtm', "ann", "Xbtm_ann", "annual Xbtm", "unknown",
    'Xbtm', "mon", "Xbtm", "monthly Xbtm", "unknown",
    'U', "ann", "U_ann", "annual U", "m/s",
    'U', "mon", "U", "monthly U", "m/s",
    'V', "ann", "V_ann", "annual V", "m/s",
    'V', "mon", "V", "monthly V", "m/s")
  
  switch(tolower(interval[1]),
         "mon" = dplyr::filter(x, .data$group %in% c("static", "mon")),
         "ann" = dplyr::filter(x, .data$group %in% c("static", "ann")),
         x)
}


brickman_database = function(path = file.path(ROOT_DATA_PATH, "brickman")){
  
  #' Read the Brickman database
  #' 
  #' @param path chr, the path to the data
  #' @return tabular database
  
  readr::read_csv(file.path(path, "database.csv"), col_types = "cccc")
}


brickman_build_database = function(path = brickman_path()){
  
  #' Build the Brickman database
  #' 
  #' @param path chr, the path to the data
  #' @return tabular database
  
  ff = list.files(path, pattern = glob2rx("*.tif"))
  ff = gsub(".tif", "", ff, fixed = TRUE) |>
    strsplit( "_", fixed = TRUE) 
  dplyr::tibble(
    scenario = sapply(ff, "[[", 1),
    year = sapply(ff, "[[", 2),
    interval = sapply(ff, "[[", 3),
    var = sapply(ff, "[[", 4)) |>
  readr::write_csv(file.path(path, "database.csv"))
}


depth_first = function(x){
  
  #' A function to reorder a stars object or data table so that depth comes first
  #' 
  #' @param x stars object or a table of data
  #' @return the input possibly reordered if it has "depth" as a variable
  
  nms = names(x)
  ix = nms == "depth"
  if (any(ix)){
   nms = c("depth", nms[!ix])
   x = dplyr::select(x, dplyr::all_of(nms))
  }
  x
}

read_brickman = function(db = brickman_database() |>
                           dplyr::filter(scenario == "PRESENT",
                                         year == "PRESENT",
                                         interval == "mon"), 
                         add = c("none", "depth", "month")[1], 
                         log_me = c("none", "depth", "Xbtm")[1],
                         path = brickman_path()){

  #' Read Brickman data given a database
  #' 
  #' Intervals are exclusive - you can't read "mon" and "ann" into the same variable.
  #' If you need two or more groups then read into two or more variables.
  #' 
  #' Years are exclusive - you can't read 2055 and 2075 into the same variable.
  #' If you need two or more years then read into two or more variables.
  #' 
  #' Scenarios are exclusive - you can't read RCP85 and RCP45 into the same variable.
  #' If you need two or more scenarios then read into two or more variables.
  #' 
  #' @param db table of database of one group type
  #' @param add str, zero or more static variable to add like "depth" and "month"
  #'  (as long as the database includes interval 'mon'). If the value is not
  #'  "depth" and/or "month", like "none", then nothing is added.
  #' @param log_me str, zero or more variable to log scale
  #' @param path str, the brickman data path
  #' @return stars array
  
  if(length(unique(db$interval)) > 1) {
    stop("please just read from one interval at a time: mon, ann or static")
  }
  
  if(length(unique(db$scenario)) > 1) {
    stop("please just read from one scenario at a time: RCP85, RCP45, PRESENT")
  }
  if(length(unique(db$year)) > 1) {
    stop("please just read from one year at a time: 2055, 2075, PRESENT")
  }  
  
  x = dplyr::rowwise(db) |>
      dplyr::group_map( 
             function(tbl, key){
               filename = file.path(path, 
                                    sprintf("%s_%s_%s_%s.tif", 
                                            tbl$scenario, 
                                            as.character(tbl$year), 
                                            tbl$interval,
                                            tbl$var))

               x = if (tbl$interval == "mon"){
                  stars::read_stars(filename) |>
                   stars::st_set_dimensions("band",
                                            values = month.abb,
                                            names = "month")
                   
                 } else {
                   stars::read_stars(filename)
                 } 
             rlang::set_names(x, tbl$var)
             }) 
  x = do.call(c, append(x, list(along = NA_integer_)))
  
  # add a month covariate if the user requests it AND it isn't already there 
  # AND the database includes mon
  if (("month" %in% tolower(add)) && 
      ("mon" %in% db$interval) && 
      !("month" %in% names(x))){
    x = c(stars_as_months(x), x, along = NA_integer_)
  } # add month?
  
  if (("depth" %in% tolower(add)) && !("depth" %in% names(x))){
    db = brickman_database()
    depth = read_brickman(db |> filter(scenario == "STATIC", var == "depth"))
    if (length(dim(x) == 3)) {
      dd = sapply(month.abb, function(mon) {depth}, simplify = FALSE)
      depth = do.call(c, append(dd, list(along = list(month = month.abb))))
    }
    x = c(depth, x, along = NA_integer_)
  } # add depth?
  
  if ("Xbtm" %in% log_me && "Xbtm" %in% names(x)){
    x = x |>
      dplyr::mutate(Xbtm = log(Xbtm, base = 10))
  }
  
  if ("depth" %in% log_me && "depth" %in% names(x)){
    x = x |>
      dplyr::mutate(depth = log(depth, base = 10))
  }
  
  
  return(x)
}

stars_as_months = function(template,
                     use = c("number", "name")[1],
                     factored = FALSE){
  
  #' Create a "month" stars object modeled after a template
  #' 
  #' @param template a stars object with one or more month bands as a 
  #'   3rd dimension
  #' @param use str, one of "number" or "name" to determine how months are identified
  #' @param factored logical, if TRUE make the attribute a factor
  #' @return a stars object with month values (possibly factored) and a month dimension

  months = stars::st_get_dimension_values(template, "month")
  if (inherits(months, "character")) months = month_as_number(months)
  
  arr = template[[1]][]  # take just the first attribute as an array
  d = dim(arr)
  mode(arr) <- "numeric"
  isna = is.na(arr)
  for (i in seq_along(months)) {
    arr[,,i] = months[i]
  }
  arr[isna] <- NA_real_
  r = template[1] 
  r[[1]][] <- arr
  r = r |>
    rlang::set_names("month")
  if(tolower(use[1]) == "number"){
      # number
      if (factored){
        r = dplyr::mutate(r, month = factor(.data$month, levels = 1:12))
      } 
    } else { 
      # name
      if(factored){
        r = dplyr::mutate(r, month = factor(month_as_name(.data$month), levels = month.abb))
      } else {
        r = dplyr::mutate(r, month = month_as_name(.data$month))
      }
    }  
  return(r)
}

extract_brickman = function(x = read_brickman(), 
                            y = gom_buoys(), 
                            form = "long") {
  
  #' Extract point data from a Brickman stars object
  #' 
  #' @param x stars object (with or without month dimension)
  #' @param y sf point data, if this has a "month" attribute then we extract by month
  #'   as long as x has a month dimension
  #' @param form chr one of either "long" (the default) or "wide" to control 
  #'   output column layout. 
  #' @param ... other arguments passed to `stars::st_extract()`
  #' @return table of variable data for each input point
  
  d = dim(x)
  if ((length(d) >= 3) && ("month" %in% names(y))) {
    return(extract_brickman_by_month(x, y, form = form))
  }
  
  n = nrow(y)
  N = floor(log10(n)+ 1)
  fmt = paste0("p%0.", N, "i")
  pnames = sprintf(fmt, seq_len(n))
  xy = sf::st_coordinates(y)
  if (length(dim(x)) == 2){
    m = stars::st_extract(x, xy)
    rownames(m) <- pnames
    p = dplyr::as_tibble(m, rownames = ".id") |>
      tidyr::pivot_longer(-dplyr::all_of(".id"),
                          values_to = "value",
                          names_to = "name")
     
  } else {
    nms = stars::st_get_dimension_values(x, 3)
    p = lapply(names(x),
     function(nm){
       this_att = x[nm]
       m = if (inherits(this_att[[1]], 'factor')){
           d = dim(this_att[[1]])
           month_lut = seq_along(month.abb)
           names(month_lut) = month.abb
           m = month_lut[as.character(this_att[[1]])]
           this_att[[1]] <- m
           stars::st_extract(this_att, xy)
         } else {
           stars::st_extract(this_att, xy)
         }
       colnames(m) <- nms
       rownames(m) <- pnames
       m |>
         dplyr::as_tibble(rownames = ".id") |>
         dplyr::mutate(name = nm, .after = 1)
     }) |>
      dplyr::bind_rows() |>
      tidyr::pivot_longer(-dplyr::all_of(c(".id", "name")),
                          names_to = "month",
                          values_to = "value")
  }
  if (tolower(form[1]) == "wide"){
    p = tidyr::pivot_wider(p,
                           names_from = "name",
                           values_from = "value")
  }
  
  p
}


extract_brickman_by_month = function(x = read_brickman(), 
                                     y = gom_buoys(), 
                                     form = "long") {
  
  #' Extract point data from a Brickman stars object
  #' 
  #' @param x stars object (with or without month dimension)
  #' @param y sf point data, if this has a "month" attribute then we extract by month
  #'   as long as x has a month dimension
  #' @param form chr one of either "long" (the default) or "wide" to control 
  #'   output column layout. 
  #' @param ... other arguments passed to `stars::st_extract()`
  #' @return table of variable data for each input point
  
  d = dim(x)
  if (length(d) != 3) stop("x must have 3rd dimension")
  if (!"month" %in% names(y)) stop("y must have 'month' as an attribute")
  
  n = nrow(y)
  N = floor(log10(n)+ 1)
  fmt = paste0("p%0.", N, "i")
  geomcol = attributes(y)$sf_column
  
  z = y |>
    dplyr::mutate(.id = sprintf(fmt, seq_len(n)), .before = 1) |>
    dplyr::group_by(month) |>
    dplyr::group_map(
      function(rows, key){
        tempx = dplyr::slice(x, "month", month.abb[key$month])
        r = stars::st_extract(tempx, sf::st_coordinates(rows)) |>
          dplyr::as_tibble()
        dplyr::bind_cols(rows, r) |>
          dplyr::relocate(dplyr::all_of(geomcol), 
                          .after = dplyr::last_col())
      }, .keep = TRUE) |>
    dplyr::bind_rows() |>
    dplyr::arrange(.id)

  if (tolower(form[1]) == "long"){
    z = z |>
      tidyr::pivot_longer(!dplyr::all_of(c("month", "class", ".id", "Month", geomcol)),
                          names_to = "name",
                          values_to = "value") |>
      dplyr::relocate(dplyr::all_of(geomcol), 
                      .after = dplyr::last_col())
  }
  z
}

read_mask = function(){
  #' Read the mask raster for the predictor data
  #' @return a stars object

  db = brickman_database() |>
  dplyr::filter(var == "mask")
  read_brickman(db) |>
    rlang::set_names("mask")
}


read_depth = function(){
  #' Read the depth raster for the predictor data
  #' @return a stars object
  
  db = brickman_database() |>
    dplyr::filter(var == "depth")
  read_brickman(db) |>
    rlang::set_names("depth")
}


assemble_brickman= function(scenario = c("RCP85", "RCP45", "PRESENT"),
                           year = c("2055", "2075", "PRESENT"),
                           vars = "all", 
                           interval = c("mon", "ann"), 
                           bb = colby_bbox(),
                           band_as_time = FALSE,
                           path = brickman_path()){
  
  #' Read, warp, subset and archive all combinations of the raw Brickman data
  #' 
  #' 
  #' @param scenario chr, one o rmore of "RCP85", "RCP45", or "PRESENT"
  #' @param year chr, year "2055" or "2075"
  #' @param vars chr, variable names (such as "SST" or "all")
  #' @param interval chr, either "ann" or "mon" (default)
  #' @param bb sf, something which provides a bounding box for subsetting
  #' @param band_as_time logical, convert band to appropriate time/date stamp
  #' @param path filepath, path to where you want data saved
  #' @return tibble database
  
  
  if(FALSE){
    scenario = c("RCP85", "RCP45", "PRESENT")
    year = c("2055", "2075", "PRESENT")
    vars = "all"
    interval = c("mon", "ann")[1]
    bb = colby_bbox()
    band_as_time = FALSE
    path = brickman_path()
  }
  
  # first load PRESENT
  # for each interval (mon and ann) of each scenario (RCP85 and (RCP45) of each year (2055 and 2075) 
  #  load (PRESENT + departure)
  
  if(!dir.exists(path)) ok = dir.create(path, recursive = TRUE)
  
  # deal with bathymetry and land mask separately
  
  for (int in c("mon", "ann")){
    vars = brickman_layers(interval = int)
    present = brickman::read_brickman(scenario = "PRESENT", 
                                      year = NA, 
                                      vars = vars |> 
                                        dplyr::filter(group != "static") |>
                                        dplyr::pull(name), 
                                      interval = int) |>
      brickman::warp_brickman(crs = sf::st_crs(bb))
    present = present[bb]
    for (nm in names(present)){
      filename = file.path(path, sprintf("%s_%s_%s_%s.tif", "PRESENT", "PRESENT", int, nm))
      ok = present[nm,,, drop = FALSE] |>
        stars::write_stars(filename)
    } # name of var
  
    for (scenario in c("RCP85", "RCP45")) {
      for (year in c("2055", "2075")){
        s = brickman::read_brickman(scenario = scenario, 
                                year = year, 
                                vars = vars |> 
                                  dplyr::filter(group != "static") |>
                                  dplyr::pull(name), 
                                interval = int) |>
          brickman::warp_brickman(crs = sf::st_crs(bb))
        s = s[bb]
        for (nm in names(s)) {
          if (nm %in% names(present)) s[[nm]] = s[[nm]] + present[[nm]]
        } # name of var
        for (nm in names(s)){
          filename = file.path(path, sprintf("%s_%s_%s_%s.tif", scenario, year, int, nm))
          ok = s[nm,,, drop = FALSE] |>
            stars::write_stars(filename)
        } # name of var
      } #year
    } # scenario
  } # int
  
  bathy = brickman::read_brickman(scenario = "PRESENT", year = NA, var = "Bathy_depth") |>
    brickman::warp_brickman(crs = sf::st_crs(bb))
  bathy = bathy[bb] |>
    stars::write_stars(file.path(path, "STATIC_STATIC_static_depth.tif"))
  mask = brickman::read_brickman(scenario = "PRESENT", year = NA, var = "land_mask") |>
    brickman::warp_brickman(crs = sf::st_crs(bb))
  mask = mask[bb] |>
    stars::write_stars(file.path(path, "STATIC_STATIC_static_mask.tif"))
  
}