#' @title Assign raster points to graph nodes
#'
#' @description This function takes a [`gGraph`] object and a raster
#' `SpatRaster`, and assigns each raster cell to the nearest graph node. 
#' The points are stored in the graph as a node attribute called
#' `raster_points`, which contains all raster values for that node. 
#' Users can then summarize these points to create new attributes.
#'
#' @param graph A [`gGraph`] object.
#' @param raster A `SpatRaster` object (from `terra`) 
#' @param layer_name Character, optional. If provided, stores the raster points in
#'   `graph@nodes.attr[[layer_name]]` instead of `raster_points.`
#' @return A [`gGraph`] object with a new node attribute containing
#'   the raster points assigned to each node.
#'
#' @details This function is memory-intensive if the raster is high-resolution and/or the
#' graph has many nodes. Users should be mindful of input sizes. Once points are
#' assigned, the user can compute summaries (mean, median, SD, thresholding)
#' to create custom attributes like "mountain" or "land". 
#'
#' @examples
#' \dontrun{
#' graph <- assignRasterPoints(rawgraph.40k, elevation_raster)
#' 
#' @export
assignRasterPoints <- function(graph, raster, layer_name = "raster_points") {
  
  if (!inherits(graph, "gGraph")) stop("graph must be a gGraph object")
  
  #TODO check that raster and graph cover similar areas?
  
  # Convert raster to data.frame
  raster.df <- as.data.frame(raster, xy = TRUE)
  colnames(raster.df) <- c("lon", "lat", "value")
  
  # Convert raster points to sf
  raster.sf <- sf::st_as_sf(raster.df, coords = c("lon", "lat"), crs = 4326)
  
  # Convert nodes to sf
  node.coords <- as.data.frame(graph@coords)
  node.coords$node.id <- seq_len(nrow(node.coords))
  nodes.sf <- sf::st_as_sf(node.coords, coords = c("lon", "lat"), crs = 4326)
  
  # Disable s2 for nearest neighbor
  sf::sf_use_s2(FALSE) #@TODO is this necessary?
  
  # Find nearest node for each raster point
  raster.sf$node.id <- sf::st_nearest_feature(
    raster.sf,
    nodes.sf
  )
  
  # create the attribute object as a list-column
  atribute <- raster.sf %>% 
    sf::st_drop_geometry() %>%
    dplyr::group_by(node.id) %>%
    tidyr::nest()
    
  all.nodes <- tibble::tibble(
    node.id = seq_len(nrow(nodes.sf))
  )
  
  atribute.full <- all.nodes %>%
    dplyr::left_join(atribute, by = "node.id")
  
  # Store in graph node attribute
  graph@nodes.attr[[layer_name]] <- atribute.full$data
  
  return(graph)
}
