#' Create a new gGraph object 
#' 
#' The generic function \code{createNewGraph} creates a new graph using a custom discrete global grid of 
#' any resolution and creates a new gGraph object of it.\cr
#'
#' @param geo_mask a tibble or a data.frame. Input must have
#' two columns giving longitudes and latitudes of locations being considered.
#' @param spacing a numeric value giving the desired spacing (in km) of the discrete
#' global grid to be constructed. dggs will then find the closest possible
#' resolution available.
#' @param attr a character vector giving names of the variables to be extracted
#' from the layer. 
#' @param method a character string indicating which method should be used to
#' compute the nodes value from the \code{attr}. Currently available options are 'any',
#' 'majority' and 'all', where the node is associated to the \code{attr} if either any,
#' the majority or all points in the cell are of the \code{attr}.
#' @param \dots further arguments to be passed to other methods. Currently not
#' used.
#' @return A \linkS4class{gGraph} object of the provided discrete global grid,
#' with nodes attributes corresponding to the variables requested in
#' \code{attr} stored in the (\code{@nodes.attr} slot).\cr
#' 

###############
## createNewGraph
###############

createNewGraph <- function(geo_mask, spacing, attr, method, ...){
  
  # construct the discrete global grid with the given spacing
  dggs <- dgconstruct(spacing=spacing, metric=TRUE, resround='down') 
  
  # get the corresponding grid cells for each point (lat-long pair)
  geo_mask$cell <- dgGEO_to_SEQNUM(dggs,geo_mask$lon,geo_mask$lat)$seqnum
  
  # add variable "land_cell" if any land is in the cell
  geo_mask <- geo_mask %>% 
    group_by(cell) %>%
    mutate(landcell = ifelse(sum(land) > 0, 1, 0)) %>%
    slice(1) %>%
    ungroup()
  
  # get boundary coordinates for cells
  grid <- dgcellstogrid(dggs,geo_mask$cell)
  
  # merge the boundery coordinates with the nodes data
  grid <- merge(grid, geo_mask, by.x="seqnum", by.y="cell")
  
  # wrap the grid at the dateline
  wrapped_grid = st_wrap_dateline(grid, options = c("WRAPDATELINE=YES","DATELINEOFFSET=180"), 
                                  quiet = TRUE)
  
  # Converting SEQNUM to GEO gives the center coordinates of the cells
  cellcenters   <- dgSEQNUM_to_GEO(dggs,geo_mask$cell)
  cellcenters_df <- data.frame(lon = cellcenters$lon, lat = cellcenters$lat)
  wrapped_grid$cellcenters <- st_as_sf(cellcenters_df, coords = c("lon", "lat"), crs = 4326)
  wrapped_grid <- wrapped_grid %>% select(seqnum, lon, lat, geometry, landcell, cellcenters)
  
  # make sure we have an sf object
  wrapped_grid_sf <- st_as_sf(wrapped_grid)
  
  # get the neighbors list of all cells
  coords <- st_coordinates(st_centroid(wrapped_grid_sf))
  #important to use a distance just above the grid diamteer km to get all neighbors
  nb <- spdep::dnearneigh(coords, 0, spacing*1.1, longlat = TRUE)  #TODO change to sf
  neighbors_list <- unclass(nb)
  
  # add cell IDs as characters (unlike the 'seqnum' which are numeric)
  wrapped_grid_sf <- wrapped_grid_sf %>%
    mutate(cell_id = as.character(seq_len(nrow(.))))
  
  # create edge list for graphNEL 
  edgeL <- lapply(seq_along(neighbors_list), function(i) {
    list(edges = as.character(wrapped_grid_sf$cell_id[neighbors_list[[i]]]))
  })
  
  # set names of edgeL to cell IDs
  names(edgeL) <- wrapped_grid_sf$cell_id
  
  # create graphNEL object
  g <- new("graphNEL",
           nodes = wrapped_grid_sf$cell_id,
           edgeL = edgeL,
           edgemode = "undirected")
  
  # create gGraph object
  coords <- wrapped_grid$cellcenters %>%
    st_coordinates() %>%
    as.data.frame()
  
  names(coords) <- c("lon", "lat")
  
  land_ggraph <- new(
    "gGraph",
    graphNEL = land_graphnel,
    coords = coords,
    nodes.attr = land_new,
    meta = list(costs = NULL, colors = NULL),
    neighbours = land_neighbours
  )
  return(land_ggraph)
}
