#' Build a geom for a wind radii for storm data to display in a map
#'
#' The wind radii data and wind radii chart provide a clearer picture of the storm structure 
#' than the simpler measurements of a storm's position and maximum winds.
#' Hurricanes can have asymmetrical wind fields, with much higher winds on one side of a 
#' storm compared to the other. Hurricane wind radii report how far winds of a certain 
#' intensity (e.g., 34, 50, or 64 knots) extended from a hurricane's center, with separate 
#' values given for the northeast, northwest, southeast, and southwest quadrants of the storm. 
#' The 34 knot radius in the northeast quadrant, for example, reports the furthest distance 
#' from the center of the storm of any location that experienced 34-knot winds in that quadrant.
#'
#' @inheritParams layer 
#' \itemize{
#'   \item \strong{data}  data.frame with columns
#'   \item storm_id  : character
#'   \item date      : POSIXct
#'   \item latitude  : numerical 
#'   \item longitude : numerical
#'   \item wind_speed: Factor w/ 3 levels "34","50","64"
#'   \item ne        : numerical
#'   \item nw        : numerical
#'   \item se        : numerical
#'   \item sw        : numerical
#' }
#'
#' @seealso [geosphere::destPoint()] computes the destination point travelling along a the shortest path
#'
#' @section Aesthetics:
#' \code{geom_hurricane} understands the following aesthetics.
#' \itemize{
#'   \item \strong{x}    longitude
#'   \item \strong{y}    latitude
#'   \item \strong{r_ne} radius north-east
#'   \item \strong{r_se} radius south-east
#'   \item \strong{r_nw} radius north-west
#'   \item \strong{r_sw} radius south-west
#'   \item scale_radii   value of scale_radii
#'   \item size          size
#'   \item fill          color of inside
#'   \item color         color of border
#'   \item alpha         transparency
#' }
#'
#' @section Depends on:
#' \enumerate{
#'   \item \code{\link[ggplot2]{layer}} in the \code{ggplot2} package.
#'   \item \code{\link[tibble]{layer}} in the \code{tibble} package.
#'   \item \code{\link[geosphere]{layer}} in the \code{geosphere} package.
#'   \item \code{\link[grid]{layer}} in the \code{grid} package.
#' }
#'
#' @examples
#' \dontrun{
#'   map_data <- ggmap::get_map(location = "Louisiana", zoom = 6, maptype = "toner-background")
#'   base_map <- ggmap(map_data, extent = "device")
#'   base_map +
#'     geom_hurricane( data = df_Ike, 
#'                     aes( x = longitude, y = latitude,
#'                          r_ne  = ne, r_se = se,
#'                          r_nw  = nw, r_sw = sw,
#'                          scale_radii = 1,
#'                          fill  = wind_speed,
#'                          color = wind_speed) 
#'                   ) +
#'     scale_color_manual( name   = "Wind speed (kts)",
#'                         values = c("red", "orange", "yellow")) +
#'     scale_fill_manual( name   = "Wind speed (kts)",
#'                        values = c("red", "orange", "yellow"))
#' }                        
#'
#' @export
#'
geom_hurricane <- function(mapping = NULL, data = NULL, stat = "identity",
                           position = "identity", na.rm = FALSE,
                           show.legend = NA, inherit.aes = TRUE, ...){
  ggplot2::layer(
    geom = Geom_Hurricane, mapping = mapping,
    data = data, stat = stat, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm,...)
  )
}

Geom_Hurricane <- ggplot2::ggproto(
  "Geom_Hurricane", 
  ggplot2::Geom,
  required_aes = c("x", "y", "r_ne", "r_se", "r_sw", "r_nw"),
  default_aes = ggplot2::aes(colour = "NA", fill = "grey", size = 0.5, linetype = 1, alpha = 0.75, scale_radii = 1),
  draw_key = ggplot2::draw_key_polygon,
  draw_group = function(data, panel_scales, coord){
    
    # parameters
    l_point_xy <- c(data[1,]$x, data[1,]$y)
    l_rad      <- c( data[1,]$r_ne, data[1,]$r_se, data[1,]$r_nw, data[1,]$r_sw )
    l_color    <- data[1,]$colour
    l_fill     <- data[1,]$fill
    l_alpha    <- data[1,]$alpha
    l_sc_radii <- data[1,]$scale_radii
    
    df_data <- tibble::tibble()
    
    # over 4 points of the compass (north-east, south-east, south-west, north-west)   
    for (i in 1:4) {
      
      # get polygon per points of the compass
      l_quad    <- ((i-1)*90):(i*90)
      l_dist    <- l_rad[i] * 1852 * l_sc_radii
      l_polygon <- geosphere::destPoint( p = l_point_xy, b=l_quad, d = l_dist )
      
      df_seg  <- tibble::tibble( x = c( l_polygon[,"lon"], l_point_xy[1] ),
                                 y = c( l_polygon[,"lat"], l_point_xy[2] )  )    
      
      df_data <- rbind( df_data, df_seg )                              
      
    }  
    
    # transform coordinates
    coords <- coord$transform( df_data, panel_scales)
    
    grid::polygonGrob( x = coords$x,
                       y = coords$y,
                       gp = grid::gpar(col = l_color, fill = l_fill, alpha = l_alpha)
    )
  }
)