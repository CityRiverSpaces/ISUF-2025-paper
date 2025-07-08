library(rcrisp)
library(dplyr)
library(lwgeom)
library(sf)

# Set input parameters
CITY_RIVERS_FILEPATH <- file.path("output", "city_rivers.csv")
SEGMENT_DIR <- file.path("output", "segments")
FEATURES_DIR <- file.path("output", "features")
OUTPUT_METRICS_FILEPATH <- file.path("output", "city_rivers_metrics.gpkg")

#' Main access point of the script
#'
#' Compute all the metrics for the (pre-computed) segments of the city rivers.
#'
#' @param city_rivers_filepath string
#' @param segments_dir string
#' @param features_dir string
#' @param output_metrics_filepath string
run <- function(city_rivers_filepath, segments_dir, features_dir,
                output_metrics_filepath) {
  # Load city river table as a data frame
  city_rivers <- read.csv(city_rivers_filepath) |>
    filter(!city_name %in% c("Paris", "London"))

  # Loop over the cities, retrieve the segments and features for the available
  # city rivers. For each segment, compute a list of metrics.
  all_metrics <- NULL
  for (n in seq_len(nrow(city_rivers))) {
    cr <- city_rivers[n, ]
    city_name <- cr$city_name
    river_name <- cr$river_name
    segments <- load_vector_data(city_name, river_name, dir = segments_dir)
    if (is.null(segments)) next
    print(paste0("Computing metrics for ", river_name, ", ", city_name))
    features <- get_features(city_name, river_name, segments,
                             dir = features_dir)
    metrics <- compute_metrics(
      streets = features[["streets"]],
      railways = features[["railways"]],
      buildings = features[["buildings"]],
      segments = features[["segments"]],
      blocks = features[["blocks"]],
      sanctuary_polygons = features[["sanctuary_polygons"]],
      river_centerline = features[["river_centerline"]],
      river_surface = features[["river_surface"]]
    )
    metrics["city_name"] <- city_name
    metrics["river_name"] <- river_name
    all_metrics <- bind_rows(all_metrics, metrics)
  }

  # Write out metrics
  st_write(all_metrics, output_metrics_filepath, quiet = TRUE, append = FALSE)
}

#' Get all features for the given city river
#'
#' Load simple features directly retrieved from OSM or precomputed via rcrisp,
#' and compute derived features. TODO: missing tessellation cells
#'
#' @param city_name string
#' @param river_name string
#' @param segments [`sf::sfc`] object
#' @param dir string
get_features <- function(city_name, river_name, segments, dir = ".") {
  # OSM features
  streets <- load_vector_data(city_name, river_name, suffix = "streets", dir = dir)
  railways <- load_vector_data(city_name, river_name, suffix = "railways", dir = dir)
  buildings <- load_vector_data(city_name, river_name, suffix = "buildings", dir = dir)
  river_centerline <- load_vector_data(city_name, river_name, suffix = "river", layer = "centerline", dir = dir)
  river_surface <- load_vector_data(city_name, river_name, suffix = "river", layer = "surface", dir = dir)

  # Derived features
  corridor <- st_union(segments)
  composite <- get_composite(buildings)
  blocks <- get_blocks(corridor, streets, railways, river_centerline)
  sanctuary_polygons <- get_sanctuary_polygons(corridor, streets)

  # Return all features
  list(
    # OSM features
    streets = streets,
    railways = railways,
    buildings = buildings,
    river_centerline = river_centerline,
    river_surface = river_surface,
    # Derived OSM features
    composite = composite,
    blocks = blocks,
    sanctuary_polygons = sanctuary_polygons,
    # CRiSp features
    corridor = corridor,
    segments = segments
  )
}

compute_metrics <- function(streets, railways, buildings, segments, blocks,
                            sanctuary_polygons, river_centerline,
                            river_surface) {
  # Set segment geometries in lat/lon, so that we can merge results for all
  # city rivers
  metrics <- st_transform(segments, "EPSG:4326")
  # Metrics specific to urban river spaces
  # Corridor segment area
  metrics["cs_a"] <- get_cs_area(segments)
  # Perimeter area ratio
  metrics["cs_par"] <- get_perimeter_area_ratio(segments)
  # Open space ratio
  metrics["cs_osr"] <- get_open_space_ratio(segments, buildings)
  # General urban form metrics
  # Length of street network
  metrics["sn_l"] <- get_network_length(segments, streets)
  # Length of rail network
  metrics["rn_l"] <- get_network_length(segments, railways)
  # Density of street network
  metrics["sn_d"] <- get_density(segments, metrics[["sn_l"]])
  # Density of rail network
  metrics["rn_d"] <- get_density(segments, metrics[["rn_l"]])
  # Number of blocks
  metrics["b_c"] <- get_number_of_blocks(segments, blocks)
  # Density of blocks
  metrics["b_d"] <- get_density(segments, metrics[["b_c"]])
  # Mean sanctuary area
  metrics["sa_a_m"] <- get_mean_sanctuary_area(segments, sanctuary_polygons)
  # Standard deviation of sanctuary area
  metrics["sa_a_sd"] <- get_sd_sanctuary_area(segments, sanctuary_polygons)
  # Sinuosity of river centerline
  metrics["rc_sin"] <- get_sinuosity(segments, river_centerline)
  # Number of river crossings
  metrics["rc_cr_n"] <-
    get_number_of_river_crossings(segments, river_centerline, streets, railways)
  # Density of river crossings
  metrics["rc_cr_d"] <-
    get_crossings_linear_density(segments, river_centerline,
                                 metrics[["rc_cr_n"]])
  # Ratio of river surface area
  metrics["rs_a_p"] <-
    get_river_surface_area_ratio(segments, river_surface)

  metrics <- metrics |>
    filter(!is.infinite(rc_sin))

  # Return computed metrics
  metrics
}

load_vector_data <- function(city_name, river_name, suffix = NULL, dir = ".",
                             layer = NULL, ext = "gpkg") {
  filepath <- get_filepath(city_name, river_name, suffix = suffix, dir = dir, ext = ext)
  if (file.exists(filepath)) {
    # For some reason, calling st_read with layer = NULL fails
    if (is.null(layer)) {
      st_read(filepath, quiet = TRUE)
    } else {
      st_read(filepath, layer = layer, quiet = TRUE)
    }
  } else {
    NULL
  }
}

get_filepath <- function(city_name, river_name, suffix = NULL, dir = ".", ext = "gpkg") {
  stem <- paste(city_name, river_name, sep = "_")
  if (!is.null(suffix)) stem <- stem <- paste(stem, suffix, sep = "_")
  filename <- paste(stem, ext, sep = ".")
  file.path(dir, filename)
}

#' Merged OSM buildings
get_composite <- function(buildings) {
  buildings |>
    st_union() |>
    st_cast("POLYGON")
}

#' Approximate OSM blocks
#'
#' Area enclosed by all streets, railways and the river centerline.
get_blocks <- function(corridor, streets, railways, river_centerline) {
  lines <- c(
    st_geometry(streets), st_geometry(railways), st_geometry(river_centerline)
  )
  st_split(corridor, lines) |>
    st_collection_extract()
}

#' Sanctuary area
#'
#' Derived from major OSM streets, railways and rivers according to Dibble et
#' al. (2019). We use the default street network elements used in rcrisp
#' ("motorway", "trunk", "primary", "secondary", "tertiary").
get_sanctuary_polygons <- function(corridor, streets) {
  lines <- streets |>
    filter(type %in% c("motorway", "trunk", "primary", "secondary", "tertiary")) |>
    st_geometry()
  st_split(corridor, lines) |>
    st_collection_extract()
}

get_cs_area <- function(segments) {
  st_area(segments)
}

get_perimeter_area_ratio <- function(segments) {
  st_length(st_boundary(segments)) / st_area(segments)
}

#' Estimate Open Space Ratio (OSR) as 1. - area_buildings / area_tot
get_open_space_ratio <- function(segments, buildings) {
  # Calculate the OSR in the nsegment-th segment
  calc_open_space_ratio <- function(nsegment) {
    segment <- segments[nsegment, ]
    area_buildings <- st_intersection(st_geometry(buildings), segment) |>
      st_as_sf() |>
      filter(st_geometry_type(x) %in% c("POLYGON", "MULTIPOLYGON")) |>
      st_area() |>
      sum()
    area_segment <- st_area(segment)
    (area_segment - area_buildings) / area_segment
  }
  sapply(seq_len(nrow(segments)), calc_open_space_ratio)
}

get_network_length <- function(segments, edges) {
  # Calculate the total network length in the nsegment-th segment
  calc_network_length <- function(nsegment) {
    segment <- segments[nsegment, ]
    st_intersection(st_geometry(edges), segment) |>
      st_as_sf() |>
      filter(st_geometry_type(x) %in% c("LINESTRING", "MULTILINESTRING")) |>
      st_length() |>
      sum()
  }
  sapply(seq_len(nrow(segments)), calc_network_length)
}

get_density <- function(segments, x) {
  x / st_area(segments)
}

get_number_of_blocks <- function(segments, blocks) {
  intersections <- st_intersects(segments, blocks)
  sapply(intersections, length)
}

get_mean_sanctuary_area <- function(segments, sanctuary_polygons) {
  intersections <- st_intersects(segments, sanctuary_polygons)
  sapply(intersections, \(n) mean(st_area(sanctuary_polygons[n])))
}

get_sd_sanctuary_area <- function(segments, sanctuary_polygons) {
  intersections <- st_intersects(segments, sanctuary_polygons)
  sapply(intersections, \(n) sd(st_area(sanctuary_polygons[n])))
}

get_sinuosity <- function(segments, river_centerline) {
  # Calculate the sinuosity in the nsegment-th segment
  calc_sinuosity <- function(nsegment) {
    segment <- segments[nsegment, ]
    river <- st_intersection(river_centerline, segment)
    river <- river[which.max(st_length(river)), ] # Take the longest river centerline
    river_length <- st_length(river)
    # get the euclidean distance between the start and end points of the river
    coords <- st_coordinates(river)
    start_point <- st_point(coords[1, ])
    end_point <- st_point(coords[nrow(coords), ])
    straight_line <-
      st_sfc(st_cast(st_linestring(rbind(start_point, end_point)),
                     "LINESTRING"), crs = st_crs(river))
    straight_length <- st_length(straight_line)

    river_length / straight_length
  }
  sapply(seq_len(nrow(segments)), calc_sinuosity)
}

get_number_of_river_crossings <- function(segments, river_centerline,
                                          streets, railways) {
  # Get the number of crossings in the nsegment-th segment
  calc_number_of_crossings <- function(nsegment) {
    segment <- segments[nsegment, ]
    river <- st_intersection(st_geometry(river_centerline), segment)
    # if (st_is_empty(river)) return(NULL)
    # Get the number of crossings with streets and railways
    crossings_streets <- st_intersects(river, streets)
    crossings_railways <- st_intersects(river, railways)
    sum(sapply(crossings_streets, length)) +
      sum(sapply(crossings_railways, length))
  }
  sapply(seq_len(nrow(segments)), calc_number_of_crossings)
}

get_crossings_linear_density <- function(segments, river_centerline, n_crossings) {
  # Calculate the linear density of crossings in the nsegment-th segment
  calc_crossings_linear_density <- function(nsegment) {
    segment <- segments[nsegment, ]
    river <- st_intersection(st_geometry(river_centerline), segment)
    river <- river[which.max(st_length(river)), ] # Take the longest river centerline
    # if (st_is_empty(river)) return(NULL)
    river_length <- st_length(river)
    n_crossings[nsegment] / river_length
  }
  sapply(seq_len(nrow(segments)), calc_crossings_linear_density)
}

get_river_surface_area_ratio <- function(segments, river_surface) {
  # Calculate the percentage of river surface area in the nsegment-th segment
  calc_river_surface_area_percentage <- function(nsegment) {
    segment <- segments[nsegment, ]
    river_area <- st_intersection(st_geometry(river_surface), segment) |>
      st_as_sf() |>
      filter(st_geometry_type(x) %in% c("POLYGON", "MULTIPOLYGON")) |>
      st_area() |>
      sum()
    area_segment <- st_area(segment)
    river_area / area_segment
  }
  sapply(seq_len(nrow(segments)), calc_river_surface_area_percentage)
}

# Call the main function
run(CITY_RIVERS_FILEPATH, SEGMENT_DIR, FEATURES_DIR, OUTPUT_METRICS_FILEPATH)

