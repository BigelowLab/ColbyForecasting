
get_year = function(date = Sys.Date()){
  
  #' A function to retrieve a dates year as a 4-digit character string
  #' 
  #' @param date Date or POSIXct class object
  #' @return the year as 4 character string

  format(date, "%Y")
}



get_user = function(strip_basename = TRUE){
  
  #' A function to retrieve the username
  #' 
  #' @param strip_basename logical, if TRUE return just the basename. 
  #'   This only has meaning if the platform is Windows, harmless
  #'   otherwise
  #' @return username
  
  u = system("whoami", intern = TRUE)
  if (strip_basename) u = basename(u)
  u
}


make_path = function(path){
  
  #' Make a path if it doesn't exist
  #' 
  #' @param path chr the path to create
  #' @return the path
  if (!dir.exists(path)) ok = dir.create(path, recursive = TRUE)
  return(path)
}



personal_data_path = function(user = get_user(),
                              year = get_year(),
                              root = ROOT_DATA_PATH){
  
  #' Get the personal data path
  #' 
  #' @param user chr, the user's name (no spaces please)
  #' @param year chr, the current year as 4-character string
  #' @param root chr, the root directory for data
  #' @return chr personal data path
  
  path = file.path(root, year, user)
  if (!dir.exists(path)) ok = dir.create(path, recursive = TRUE)
  return(path)
}


data_path = function(...,  root = ROOT_DATA_PATH){
  
  #' Get the data path
  #' 
  #' @param what chr path and file segments to append to the root path using the
  #'   system file separator
  #' @param root chr, the root personal data directory
  #' @return some directory in the personal data path
  
  file.path(root, ...)
}


env_path = function(root = ROOT_DATA_PATH){
  
  #' Retrieve the path to the brickman data
  #' 
  #' @param root chr, the root global data directory
  #' @return the path to the brickman data directory
  
  file.path(root, "brickman")
}



coastline_path = function(root = ROOT_DATA_PATH){
  
  #' Retrieve the path to the coastline data
  #' 
  #' @param root chr, the root global data directory
  #' @return the path to the coastline data directory
  
  file.path(root, "coastline")
}

obis_path = function(root = ROOT_DATA_PATH){
  
  #' Retrieve the path to the obis data
  #' 
  #' @param root chr, the root personal data directory
  #' @return the path to the brickman config file
  
  path = file.path(root, "obis")
  if (!dir.exists(path)) ok = dir.create(path, recursive = TRUE)
  return(path)
}