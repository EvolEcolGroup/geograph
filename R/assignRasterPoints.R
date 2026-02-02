#' Assign raster points to graph nodes
#'
#' @description
#' \code{assignRasterPoints} takes a \linkS4class{gGraph} object and a raster
#' \code{SpatRaster}, and assigns each raster cell to the nearest graph node. 
#' The points are stored in the graph as a node attribute called
#' \code{raster_points}, which contains all raster values for that node. 
#' Users can then summarize these points to create new attributes.
#'
#' @param graph A \linkS4class{gGraph} object.
#' @param raster A \code{SpatRaster} object (from \code{terra}) 
#' @param layer_name Character, optional. If provided, stores the raster points in
#'   \code{graph@nodes.attr[[layer_name]]} instead of \code{raster_points}.
#' @return A \linkS4class{gGraph} object with a new node attribute containing
#'   the raster points assigned to each node.
#'
#' @details
#' This function is memory-intensive if the raster is high-resolution and/or the
#' graph has many nodes. Users should be mindful of input sizes. Once points are
#' assigned, the user can compute summaries (mean, median, SD, thresholding)
#' to create custom attributes like "mountain" or "land". 
#'
#' @examples
#' \dontrun{
#' graph <- assignRasterPoints(rawgraph.40k, elevation_raster)
#' # Now each node has a list-column raster_points with all assigned raster points
#' # You can compute node-level attributes, e.g.:
#' node_sd <- sapply(graph@nodes.attr$raster_points, function(df) sd(df$elevation))
#' graph@nodes.attr$sd_elevation <- node_sd
#' }
#'
#' @export
assignRasterPoints <- function(graph, raster, layer_name = "raster_points") {
  
  if (!inherits(graph, "gGraph")) stop("graph must be a gGraph object")
  
  #@TODO check that raster and graph cover similar areas?
  
  # Convert raster to data.frame
  raster_df <- as.data.frame(raster, xy = TRUE)
  colnames(raster_df) <- c("lon", "lat", "value")
  
  # Convert raster points to sf
  raster_sf <- sf::st_as_sf(raster_df, coords = c("lon", "lat"), crs = 4326)
  
  # Convert nodes to sf
  node_coords <- as.data.frame(graph@coords)
  node_coords$node_id <- seq_len(nrow(node_coords))
  nodes_sf <- sf::st_as_sf(node_coords, coords = c("lon", "lat"), crs = 4326)
  
  # Disable s2 for nearest neighbor
  sf::sf_use_s2(FALSE) #@TODO is this necessary?
  
  # Find nearest node for each raster point
  raster_sf$node_id <- st_nearest_feature(
    raster_sf,
    nodes_sf
  )
  
  # create the attribute object as a list-column
  atribute <- raster_sf %>% 
    sf::st_drop_geometry() %>%
    dplyr::group_by(node_id) %>%
    tidyr::nest()
  
  # Store in graph node attribute
  graph@nodes.attr[[layer_name]] <- atribute$data
  
  return(graph)
}
