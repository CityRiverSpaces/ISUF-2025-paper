# 20250616 Updated by Yehan Wu
# 20250618 Refactored by Claudiu Forgaci

library(sf)
library(dplyr)
library(ggplot2)
library(ggspatial)
library(maptiles)
library(leaflet)
library(tidyr)
library(factoextra)
library(FactoMineR)
library(car)
library(corrplot)
library(writexl)
library(webshot2)

# Set input parameters
METRICS_FILEPATH <- file.path("output", "city_rivers_metrics.gpkg")
TOP5_OUTPUT_DIR <- file.path("output", "top5")

run <- function(metrics_filepath, top5_output_dir) {
  # Read metrics generated in "scripts/compute_metrics.R"
  metrics <- st_read(metrics_filepath)

  # Create id column
  metrics$id <- 1:nrow(metrics)

  # Print the number of segments
  cat("Number of segments in the dataset:", nrow(metrics), "\n")

  vars <- metrics[, c("cs_a", "cs_par", "cs_osr", "sn_l", "rn_l", "sn_d",
                      "rn_d", "b_c", "b_d", "sa_a_m", "sa_a_sd", "rc_sin",
                      "rc_cr_n", "rc_cr_d", "rs_a_p", "id")]
  vars_df <- st_drop_geometry(vars)
  vars_clean <- vars_df |>
    filter(cs_par <= 0.02, # Remove outlier with high cs_par
                           # TODO Note that 0.2 seems to be a better threshold;
                           # It only removes the extreme case of Geneva, L'Arve
           rc_sin <= 10)   # Remove outlier with incorrect rc_sin
  vars_clean_nona <-
    vars_clean[complete.cases(vars_clean), ]  # TODO Check why sa_a_sd has NAs
  vars_new <- vars_clean_nona |>
    select(-b_c, -b_d, -sn_l, -rc_cr_n, -rn_l)  # TODO Check after changed input
  vars_scaled <- scale(select(vars_new, -id))

  # Determine K
  k_optimal <- fviz_nbclust(vars_scaled, kmeans, method = "silhouette",
                            k.max = 9, nstart = 20)
  set.seed(0)
  k <- which.max(k_optimal$data$y)

  # Run K-means clustering on the standardized data
  kmeans_result <- kmeans(vars_scaled, centers = k, nstart = 20)

  # Add cluster labels
  cluster_df <- data.frame(
    id = vars_clean_nona$id,
    cluster = as.factor(kmeans_result$cluster)
  )
  metrics_clustered <- left_join(metrics, cluster_df, by = "id")

  cluster_colors <- c("1" = "tomato1", "2" = "gold1", "3" = "forestgreen", "4" = "cornflowerblue", "5" = "mediumpurple1")

  # Visualize and save to file the location of clustered segments
  viz_locations(metrics_clustered)

  # Save the clustered metrics to file
  save_cluster_centers(kmeans_result, vars_scaled)

  # Visualize PCA
  vis_pca_2d(vars_scaled, kmeans_result, cluster_colors)

  # Save top 5 samples for each cluster
  top5_samples <- get_top5_samples(metrics, metrics_clustered, kmeans_result,
                                   vars_scaled, vars_clean_nona,
                                   top5_output_dir)
  top5_proj <- st_transform(top5_samples, 3857)

  # st_write(top5_proj,
  #          file.path(top5_output_dir, "top5_proj.gpkg"),
  #          quiet = TRUE, append = FALSE)

  export_cluster_examples(top5_proj, top5_output_dir)

  # # Save top 5 samples for each cluster
  # save_top5_samples(top5_samples, top5_proj, cluster_colors, top5_output_dir)
}

viz_locations <- function(metrics_clustered) {
  metrics_centroids <- metrics_clustered %>%
    filter(!is.na(cluster)) %>%
    st_transform(3857) %>%
    st_centroid()  # use centroid point

  # base bbox
  bbox_all <- st_bbox(metrics_centroids)

  p <- ggplot() +
    annotation_map_tile(type = "cartolight", zoom = 5) +
    geom_sf(data = metrics_centroids,
            # aes(color = cluster),
            color = "orange",
            size = 1.5, alpha = 0.5, show.legend = TRUE) +
    # scale_color_manual(values = cluster_colors, name = "Cluster") +
    coord_sf(xlim = c(bbox_all["xmin"], bbox_all["xmax"]),
             ylim = c(bbox_all["ymin"], bbox_all["ymax"]),
             expand = FALSE) +
    theme_void() +
    annotation_scale(location = "br", width_hint = 0.2, line_width = 0.1) +
    theme(legend.position = "right")

  p
  ggsave(filename = "cluster_type_locations.png",
         plot = p,
         width = 8, height = 8, dpi = 300)
}

save_cluster_centers <- function(kmeans_result, vars_scaled) {
  # Get the cluster centers (in standardized form)
  scaled_centers <- round(kmeans_result$centers, 4)

  # Print them
  print("Cluster centers (standardized):")
  print(round(scaled_centers, 2))

  # Convert the centers back to original scale: x * SD + mean
  original_centers <- t(apply(
    scaled_centers, 1,
    function(x) x * attr(vars_scaled, "scaled:scale") + attr(vars_scaled, "scaled:center")
  ))

  original_centers <- round(original_centers, 5)

  # Print the real-world values
  print("Cluster centers (original):")
  print(original_centers)

  cluster_center_df <- as.data.frame(original_centers)
  write_xlsx(cluster_center_df, "output/cluster_centers.xlsx")
}

vis_pca_2d <- function(vars_scaled, kmeans_result, cluster_colors) {
  # Run PCA
  pca_result <- PCA(vars_scaled, graph = FALSE)

  # Biplot visual
  p <- fviz_pca_biplot(pca_result,
                       geom.ind = "point",
                       col.ind = as.factor(kmeans_result$cluster),
                       addEllipses = TRUE,
                       label = "var",
                       col.var = "black",
                       repel = TRUE,
                       legend.title = "Cluster")+ scale_color_manual(values = cluster_colors)
  p
  ggsave(filename = "output/PCA.png",
         plot = p,
         width = 8, height = 8, dpi = 300)
}

get_top5_samples <- function(metrics, metrics_clustered, kmeans_result,
                             vars_scaled, vars_clean_nona, output_dir) {
  # Get the cluster center for each row, based on its assigned cluster
  assigned_centers <- kmeans_result$centers[kmeans_result$cluster, ]

  # Compute Euclidean distance from each point to its own cluster center
  cluster_dist <- sqrt(rowSums((vars_scaled - assigned_centers)^2))

  # Add to the spatial data
  cluster_df <- data.frame(
    id = vars_clean_nona$id,
    cluster = as.factor(kmeans_result$cluster),
    dist_to_center = cluster_dist
  )

  metrics_clustered <- left_join(metrics, cluster_df, by = "id")

  # Prepare top5 samples with rank
  top5_samples <- metrics_clustered %>%
    filter(!is.na(cluster)) %>%
    group_by(cluster) %>%
    arrange(dist_to_center) %>%
    slice_head(n = 5) %>%
    mutate(rank = row_number()) %>%
    ungroup()
}

save_top5_samples <- function(top5_samples, top5_proj, cluster_colors,
                              output_dir) {
  # Create a directory for top5 samples
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  # Determine unified square bbox size
  max_poly <- top5_proj[which.max(st_area(top5_proj)), ]
  bbox_max <- st_bbox(max_poly)
  bbox_size <- max(bbox_max["xmax"] - bbox_max["xmin"],
                   bbox_max["ymax"] - bbox_max["ymin"]) * 1.1
  half_bbox <- bbox_size / 2

  # Loop through all top5 samples
  for (i in 1:nrow(top5_proj)) {
    seg_proj <- top5_proj[i, ]
    seg <- top5_samples[i, ]

    cluster_val <- seg$cluster
    id_val <- seg$id
    rank_val <- seg$rank
    fill_col <- cluster_colors[[as.character(cluster_val)]]

    center <- st_coordinates(st_centroid(seg_proj))
    cx <- center[1]
    cy <- center[2]

    bbox_fixed <- c(
      xmin = cx - half_bbox, xmax = cx + half_bbox,
      ymin = cy - half_bbox, ymax = cy + half_bbox
    )

    label_text <- paste0(seg$city_name, ", ", seg$river_name)

    p <- ggplot() +
      annotation_map_tile(type = "osm", zoom = 15) +
      geom_sf(data = seg_proj, fill = fill_col, alpha = 0.3,
              color = "white", size = 0.8) +
      coord_sf(xlim = c(bbox_fixed["xmin"], bbox_fixed["xmax"]),
               ylim = c(bbox_fixed["ymin"], bbox_fixed["ymax"]),
               expand = FALSE) +
      theme_void() +
      labs(caption = label_text) +
      theme(plot.caption = element_text(hjust = 0, size = 28,
                                        face = "bold", margin = margin(t = 10))) +
      annotation_scale(location = "br", width_hint = 0.2, line_width = 0.1)

    p
    file_name <- sprintf("cluster%s_rank%s_id%s.png", cluster_val, rank_val, id_val)
    ggsave(filename = file.path(output_dir, file_name),
           plot = p, width = 8, height = 8, dpi = 200)
  }
}

export_cluster_examples <- function(top5_proj, output_dir) {
  top5_city_names <- top5_proj$city_name
  top5_city_names <- top5_proj$city_name

  # par(mar = c(5, 4, 8, 2))

  for (i in seq_along(top5_proj)) {
    streets <- st_read(paste0("output/features/",
                              top5_proj[i,]$city_name, "_",
                              top5_proj[i,]$river_name, "_streets.gpkg")) |>
      st_set_precision(1e7)
    railways <- st_read(paste0("output/features/",
                               top5_proj[i,]$city_name, "_",
                               top5_proj[i,]$river_name, "_railways.gpkg")) |>
      st_set_precision(1e7)
    river <- st_read(paste0("output/features/",
                            top5_proj[i,]$city_name, "_",
                            top5_proj[i,]$river_name, "_river.gpkg"))
    buildings <- st_read(paste0("output/features/",
                                top5_proj[i,]$city_name, "_",
                                top5_proj[i,]$river_name, "_buildings.gpkg"))
    segment <- st_transform(top5_proj[i,], st_crs(river))

    streets <- st_intersection(streets, segment)
    railways <- st_intersection(railways, segment)
    river <- st_intersection(river, segment)
    buildings <- st_intersection(buildings, segment)

    filename <- paste0(output_dir, "/",
                       top5_proj[i,]$cluster, "_",
                       top5_proj[i,]$rank, "_",
                       top5_proj[i,]$city_name, "_",
                       top5_proj[i,]$river_name)
    png(filename = paste0(filename, ".png"), width = 1200, height = 1200, bg = NA)
    plot(segment$geom)
        #  main = paste0(top5_proj[i,]$city_name, "\n", top5_proj[i,]$river_name),
        #  sub = paste0("Type: ", top5_proj[i,]$cluster, "\n",
                      # "Rank: ", top5_proj[i,]$rank),
        #  cex.main = 4, cex.sub = 3)
    plot(river$geom, col = "blue", lwd = 3, add = TRUE)
    plot(railways$geom, lwd = 0.5, col = "darkgreen", add = TRUE)
    plot(streets$geom, lwd = 0.5, add = TRUE)
    plot(buildings$geom, border = NA, col = "orange", add = TRUE)
    plot(segment$geom, border = "red", lwd = 2, add = TRUE)

    dev.off()
  }
}

# run ----
run(METRICS_FILEPATH, TOP5_OUTPUT_DIR)
