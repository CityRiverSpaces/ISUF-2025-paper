library(rcrisp)
library(dplyr)
library(sf)

# Set input parameters
CITY_RIVERS_FILEPATH <- file.path("output", "city_rivers.csv")
SEGMENTS_DIR <- file.path("output", "segments")
OUTPUT_FEATURES_DIR <- file.path("output", "features")
FORCE_DOWNLOAD <- FALSE

#' Main access point of the script
#'
#' @param city_rivers_filepath string
#' @param segment_dir string
#' @param output_feature_dir string
#' @param force_download bool
run <- function(city_rivers_filepath, segment_dir, output_feature_dir,
                force_download = FALSE) {
  # Load city river table as a data frame
  city_rivers <- read.csv(city_rivers_filepath)

  # Loop over the cities and retrieve the features for the available segments
  for (n in seq_len(nrow(city_rivers))) {
    cr <- city_rivers[n, ]
    city_name <- cr$city_name
    river_name <- cr$river_name
    bb <- st_bbox(c(xmin = cr$xmin,
                    xmax = cr$xmax,
                    ymin = cr$ymin,
                    ymax = cr$ymax),
                  crs = "EPSG:4326")
    segments <- load_segments(city_name, river_name, segment_dir)
    if (is.null(segments)) next
    retrieve_features(city_name = city_name,
                      river_name = river_name,
                      bb = bb,
                      segments = segments,
                      out_dir = output_feature_dir,
                      force_download = force_download)
  }
}

#' Load the delineation of a city
#'
#' @param city_name string
#' @param river_name string
#' @param segment_dir string
#' @return [`sf::sfc`] or NULL if the segment file does not exist
load_segments <- function(city_name, river_name, segment_dir) {
  # Determine file path
  filepath <- file.path(segment_dir,
                        sprintf("%s_%s.gpkg", city_name, river_name))

  # Load segment data if file is present
  if (file.exists(filepath)) {
    st_read(filepath, quiet = TRUE)
  } else {
    NULL
  }
}

#' Retrieve features for the given city
#'
#' @param city_name string
#' @param river_name string
#' @param bb sf bbox object
#' @param segments sf sfc object
#' @param out_dir string
#' @param force_download bool
retrieve_features <- function(city_name, river_name, bb, segments, out_dir,
                              force_download = FALSE) {
  dir.create(out_dir, showWarnings = FALSE)
  corridor <- st_union(segments)
  corridor_latlon <- st_transform(corridor, "EPSG:4326")
  crs <- st_crs(corridor)
  stem <- sprintf("%s_%s", city_name, river_name)
  get_river(bb = bb,
            river_name = river_name,
            filepath = file.path(out_dir, sprintf("%s_river.gpkg", stem)),
            crs = crs,
            force_download = FALSE)
  get_streets(aoi = corridor_latlon,
              filepath = file.path(out_dir, sprintf("%s_streets.gpkg", stem)),
              crs = crs,
              force_download = force_download)
  get_railways(aoi = corridor_latlon,
               filepath = file.path(out_dir, sprintf("%s_railways.gpkg", stem)),
               crs = crs,
               force_download = force_download)
  get_buildings(aoi = corridor_latlon,
                filepath = file.path(out_dir, sprintf("%s_buildings.gpkg", stem)),
                crs = crs,
                force_download = force_download)
}

get_river <- function(bb, river_name, filepath, crs = crs, force_download = FALSE) {
  if (file.exists(filepath)) return()
  river <- get_osm_river(bb, river_name, crs = crs, force_download = force_download)
  st_write(river$centerline, filepath, layer = "centerline", append = FALSE, quiet = TRUE)
  st_write(river$surface, filepath, layer = "surface", append = TRUE, quiet = TRUE)
}

get_streets <- function(aoi, filepath, crs = NULL, force_download = FALSE) {
  if (file.exists(filepath)) return()
  streets <- get_osm_streets(aoi, crs = crs, highway_values = "", force_download = force_download)
  st_write(streets, filepath, quiet = TRUE)
}

get_railways <- function(aoi, filepath, crs = NULL, force_download = FALSE) {
  if (file.exists(filepath)) return()
  railways <- get_osm_railways(aoi, crs = crs, railway_values = "", force_download = force_download)
  st_write(railways, filepath, quiet = TRUE)
}

get_buildings <- function(aoi, filepath, crs = NULL, force_download = FALSE) {
  if (file.exists(filepath)) return()
  buildings <- get_osm_buildings(aoi, crs = crs, force_download = force_download)
  st_write(buildings, filepath, quiet = TRUE)
}

# Call the main function
run(CITY_RIVERS_FILEPATH, SEGMENTS_DIR, OUTPUT_FEATURES_DIR, FORCE_DOWNLOAD)
