# Scripts

* [`retrieve_features.R`](./retrieve_features.R): Using the segments (corridor) geometries, retrieve all the basic OSM features to calculate the metrics: streets and railways (all hyerarchy levels), buildings, the river surface geometry.

* [`compute_metrics.R`](./compute_metrics.R): Estimate the metrics for all the city rivers and segments, and save output to a GPKG file (see output [dataset](../output/city_rivers_metrics.gpkg)).

* [`cluster.R`](./cluster.R):

* [`export_cluster_examples.R`](./export_cluster_examples.R):