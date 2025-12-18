#' Create a new gGraph object 
#' 
#' The function \code{createNewGraph} creates a new graph using a custom discrete global grid of 
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
  
  stopifnot(
    is.data.frame(geo_mask),
    all(c("lon", "lat", attr) %in% names(geo_mask)),
    method %in% c("any", "majority", "all"),
    is.numeric(spacing),
    spacing > 0
  )
  
  # construct the discrete global grid with the given spacing
  dggs <- dggridR::dgconstruct(spacing=spacing, metric=TRUE, resround='down') 
  
  # get the corresponding grid cells for each point (lat-long pair)
  geo_mask$cell <- dggridR::dgGEO_to_SEQNUM(dggs, geo_mask$lon, geo_mask$lat)$seqnum
  
  # aggregate the attribute values for each cell based on the chosen method
  geo_mask <- geo_mask %>%
    dplyr::group_by(cell) %>%
    dplyr::mutate(
      value = dplyr::case_when(
        method == "any"      ~ as.integer(any(.data[[attr]] > 0)),
        method == "majority" ~ as.integer(mean(.data[[attr]] > 0, na.rm = TRUE) > 0.5),
        method == "all"      ~ as.integer(all(.data[[attr]] > 0))
      )
    ) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup()
  
  
  # get boundary coordinates for cells
  grid <- dggridR::dgcellstogrid(dggs, geo_mask$cell)
  
  # merge the boundary coordinates with the nodes data
  grid <- merge(grid, geo_mask, by.x="seqnum", by.y="cell")
  
  # wrap the grid at the dateline
  wrapped_grid = sf::st_wrap_dateline(grid, options = c("WRAPDATELINE=YES","DATELINEOFFSET=180"), 
                                  quiet = TRUE)
  
  # Converting SEQNUM to GEO gives the center coordinates of the cells
  cellcenters   <- dggridR::dgSEQNUM_to_GEO(dggs, geo_mask$cell)
  cellcenters_df <- data.frame(lon = cellcenters$lon, lat = cellcenters$lat)
  wrapped_grid$cellcenters <- st_as_sf(cellcenters_df, coords = c("lon", "lat"), crs = 4326)
  wrapped_grid <- wrapped_grid %>% select(seqnum, lon, lat, geometry, all_of(attr), cellcenters)
  
  # make sure we have an sf object
  wrapped_grid_sf <- st_as_sf(wrapped_grid)
  
  nodes_attr <- wrapped_grid_sf |>
    sf::st_drop_geometry() |>
    dplyr::select(seqnum, all_of(attr))
  
  # get the neighbors list of all cells
  coords <- st_coordinates(st_centroid(wrapped_grid_sf))
  
  #important to use a distance just above the grid diamteer km to get all neighbors
  nb <- spdep::dnearneigh(coords, 0, (spacing*1.5), longlat = TRUE)  #TODO change to sf
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
    sf::st_coordinates() %>%
    as.data.frame()
  
  names(coords) <- c("lon", "lat")
  
  new_ggraph <- new(
    "gGraph",
    graphNEL = g,
    coords = coords,
    nodes.attr = nodes_attr,
    meta = list(costs = NULL, colors = NULL),
    neighbours = neighbors_list
  )
  
  return(new_ggraph)
  
}
