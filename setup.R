#' This file is used to check for needed R packages (from CRAN and github). If the 
#' packages are installed locally then they are simply loaded. If they are 
#' not installed then they are installed first and then loaded.

# Here we list the packages by name, CRAN is easier than GITHUB
packages = list(
  CRAN = c("remotes", "usethis", "ggplot2", "readr", "tidyr", "tidymodels",  
           "imager", "stars", "rnaturalearth", "robis", "httr", "yaml", "vip",
           "spatialsample", "workflowsets", "effectplots", "ranger", "ggokabeito",
           "bundle", "butcher",  "tidysdm", "sf", "dplyr", "patchwork", "pdp"),
  GITHUB = list(
    maxnet = c(repos = "BigelowLab/maxnet", ref = "main"),
    ColbyForecastingDocs = c(repos = "BigelowLab/ColbyForecastingDocs", ref = "main"))
)

# And here we check for prior installations and install locally as needed
installed = installed.packages() |> rownames()
if ("CRAN" %in% names(packages)){
  ix = packages$CRAN %in% installed
  for (package in packages$CRAN[!ix]) {
    install.packages(package)
  }
}

if ("GITHUB" %in% names(packages)){
ix = names(packages$GITHUB) %in% installed
  for(package in names(packages$GITHUB)[!ix]) {
    remotes::install_github(getElement(packages$GITHUB[[package]], "repos"),
                            ref = getElement(packages$GITHUB[[package]], "ref"))
  }
}


# Here we simply load each package form the library of packages
suppressPackageStartupMessages({
  for (package in packages$CRAN) library(package, character.only = TRUE)
  for (package in names(packages$GITHUB)) library(package, character.only = TRUE)
})

# this is a hack because one of these packages is overriding the dplyr::slice method
slice <- dplyr::slice

# We do this with the assumption that the user has cloned to their home
# directory.
HOME <- list.files("~", full.names = TRUE) |>
  getElement(1) |>
  dirname()
# Next we check the 'functions' directory for ".R" files and source those
for (f in list.files(file.path(HOME, "ColbyForecasting/functions"), 
                     pattern = glob2rx("*.R"), full.names = TRUE)) {
  source(f, echo = FALSE)
}

# Finally set path to the data hopefully as a sibling to the project directory
# The data directory has top level subdirectories ("buoys", "coast", "brickman")
# that contain data used by all, and to which you wil add your own data.
ROOT_DATA_PATH = "~/ColbyForecasting_data"
if (!dir.exists(ROOT_DATA_PATH)) {
  ok = dir.create(ROOT_DATA_PATH, recursive = TRUE)
  ok = unzip("/opt/ColbyForecasting_data.zip",
             exdir = ROOT_DATA_PATH,
             junkpaths = FALSE)
  macosx_junk = file.path(ROOT_DATA_PATH, "__MACOSX") 
  if (dir.exists(macosx_junk)) unlink(macosx_junk, recursive = TRUE, force = TRUE)
}

