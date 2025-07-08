library(sf)
top5_proj <- st_read("output/top5/top5_proj.gpkg")

top5_proj$city_name

top5_city_names <- top5_proj$city_name
top5_city_names <- top5_proj$city_name

par(mar = c(5, 4, 8, 2))

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

  plot(segment$geom,
       main = paste0(top5_proj[i,]$city_name, "\n", top5_proj[i,]$river_name),
       sub = paste0("Type: ", top5_proj[i,]$cluster, "\n",
                    "Rank: ", top5_proj[i,]$rank),
       cex.main = 4, cex.sub = 3)
  plot(river$geom, col = "blue", lwd = 3, add = TRUE)
  plot(railways$geom, lwd = 0.5, col = "darkgreen", add = TRUE)
  plot(streets$geom, lwd = 0.5, add = TRUE)
  plot(buildings$geom, border = NA, col = "orange", add = TRUE)
  plot(segment$geom, border = "red", lwd = 2, add = TRUE)

  # save last plot as image
  filename <- paste0("output/top5_examples/",
                     top5_proj[i,]$cluster, "_",
                     top5_proj[i,]$rank, "_",
                     top5_proj[i,]$city_name, "_",
                     top5_proj[i,]$river_name)
  dev.copy(png, filename = paste0(filename, ".png"), width = 1200, height = 1200)
  dev.off()
}
