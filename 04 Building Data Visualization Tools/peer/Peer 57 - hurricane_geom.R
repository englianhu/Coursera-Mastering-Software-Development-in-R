#' Hurricane Wind Radii Plot
#'
#' The hurricane geom creates a plot showing how far from the center
#' of the storm winds of a certain speed (e.g. 30, 50, 64 knots) were
#' observed at a certain time.
#'
#' Hurricane winds can be asymmetrical, with more intense winds
#' extending different distances in different directions. The
#' hurricane wind radii plot shows these distances in each of four
#' quadrants (NE, NW, SE, SW).
#'
#' @param mapping Set of aesthetic mappings specified by aes.
#'
#' @param data The hurricane wind data to be plotted. See the example
#'     for details.
#'
#' @param stat The statistical transformation to use on the data for this layer, as a string.
#'
#' @param position Position adjustment, either as a string, or the 
#'     result of a call to a position adjustment function.
#'
#' @param na.rm If FALSE (the default), removes missing values with a
#'     warning. If TRUE silently removes missing values.
#'
#' @param show.legend logical. Should this layer be included in the
#'     legends? NA, the default, includes if any aesthetics are
#'     mapped. FALSE never includes, and TRUE always includes.
#'
#' @param inherit.aes If FALSE, overrides the default aesthetics,
#'     rather than combining with them.
#'
#' @section Aesthetics
#'
#' @export dplyr %>%
#' @export dplyr filter
#' @export dplyr mutate
#' @export dplyr select
#' @export lubridate ymd_h
#' @export tidyr gather
#' @export tidyr separate
#' @export tidyr spread
#' @export tidyr unite
#' 
#' \code{geom_hurricane} understands the following aesthetics
#' (required aesthetics are in bold):
#'
#' \\itemize{
#'   \\item \\strong{x}
#'   \\item \\strong{y}
#'   \\item \\strong{r_ne}
#'   \\item \\strong{r_se}
#'   \\item \\strong{r_sw}
#'   \\item \\strong{n_nw}
#'   \\item alpha
#'   \\item color
#'   \\item fill
#'   \\item scale_radii
#' }
#'
#' 
#' # To plot hurricane wind radii using geom_hurricane, you
#' # will need a data frame in the following format:
#' @examples
#' \dontrun{
#' storm_observation <- data.frame(
#'     latitude = c(29.5, 29.5, 29.5),
#'     longitude = c(-89.6, -89.6, -89.6),
#'     wind_speed = c("34", "50", "64"),
#'     ne = c(200, 120, 90),
#'     se = c(200, 120, 90),
#'     sw = c(150, 75, 60),
#'     nw = c(100, 75, 60)
#' )
#'}
#'
#' @examples
#' \dontrun{
#' library(ggmap)
#' get_map("Louisiana", zoom = 6, maptype = "toner-background") %>%
#'   ggmap(extent = "device") +
#'   geom_hurricane(data = storm_observation,
#'                  aes(x = longitude, y = latitude,
#'                      r_ne = ne, r_se = se, r_nw = nw, r_sw = sw,
#'                      fill = wind_speed, color = wind_speed)) +
#'   scale_color_manual(name = "Wind speed (kts)",
#'                      values = c("red", "orange", "yellow")) +
#'   scale_fill_manual(name = "Wind speed (kts)",
#'                     values = c("red", "orange", "yellow"))
#'}
#' # You can reduce the plot to a percentage of the full wind radii
#' # using the optional \code{scale_radii} aesthetic. For example,
#' # \code{scale_radii = 0.8)} will plot the wind radii at 80% of
#' # their actual valuses. (The default value is 1.)

#import library for piping operation
library(dplyr)

#import library for do.call glist call
library(grid)

GeomHurricane <- ggplot2::ggproto("GeomHurricane", ggplot2::Geom,
                         required_aes = c("x", "y", "r_ne", "r_se", "r_sw", "r_nw"),
                         default_aes =  ggplot2::aes(color = NA, fill = NA, alpha = 0.8, scale_radii = 1),
                         draw_key = ggplot2::draw_key_polygon,
                         draw_group = function(data, panel_scales, coord) {
                           coords <- coord$transform(data, panel_scales)
                           locations <- c(data$x, data$y)
                           quadrant_grob <- function(quadrant, bearing) {
                             radius <- coords$scale_radii * coords[quadrant] * 1852 # nm to meters
                             quadrants <- geosphere::destPoint(locations, b = bearing, radius)
                             quadrants_df <- dplyr::data_frame(x = quadrants[, 1], y = quadrants[, 2])
                             quadrants_df <- rbind(locations, quadrants_df, locations)
                             quadrants_scaled <- coord$transform(quadrants_df, panel_scales)
                             grid::polygonGrob(x = quadrants_scaled$x,
                                               y = quadrants_scaled$y,
                                               gp = grid::gpar(col = coords$colour,
                                                               fill = coords$fill,
                                                               alpha = coords$alpha))
                           }
                           
                           grobs <- list()
                           grobs$ne <- quadrant_grob("r_ne", 0:90)
                           grobs$se <- quadrant_grob("r_se", 90:180)
                           grobs$sw <- quadrant_grob("r_sw", 180:270)
                           grobs$nw <- quadrant_grob("r_nw", 270:360)
                           do.call("gList", grobs)
                         })


# Reading Hurricane data
ext_tracks_widths <- c(7, 10, 2, 2, 3, 5, 5, 6, 4, 5, 4, 4, 5, 3, 4, 3, 3, 3, 4, 3, 3, 3, 4, 3, 3, 3, 2, 6, 1)
ext_tracks_colnames <- c("storm_id", "storm_name", "month", "day", "hour", "year", "latitude", "longitude", "max_wind", "min_pressure", "rad_max_wind", "eye_diameter", "pressure_1", "pressure_2",
                         paste("radius_34", c("ne", "se", "sw", "nw"), sep = "_"),
                         paste("radius_50", c("ne", "se", "sw", "nw"), sep = "_"),
                         paste("radius_64", c("ne", "se", "sw", "nw"), sep = "_"),
                         "storm_type", "distance_to_land", "final")

ext_tracks <- readr::read_fwf("ebtrk_atlc_1988_2015.txt", readr::fwf_widths(ext_tracks_widths, ext_tracks_colnames), na = "-99")

# Tidy the dataset into "long" format subset hurricane data to a single observation
storm_observation <- ext_tracks %>% 
  tidyr::unite(date, year, month, day, hour) %>%
  dplyr::mutate(date = lubridate::ymd_h(date)) %>% 
  dplyr::mutate(longitude = -longitude) %>%
  dplyr::mutate(storm_id = paste(storm_name,lubridate::year(date), sep="-")) %>%
  tidyr::gather(wind_speed1, radii, c(radius_34_ne, radius_50_ne, radius_64_ne,radius_34_nw, radius_50_nw, radius_64_nw,radius_34_se, radius_50_se, radius_64_se,radius_34_sw, radius_50_sw, radius_64_sw)) %>%
  tidyr::separate(wind_speed1, c("radius", "wind_speed", "quadrant")) %>%
  subset(as.character(date) == "2008-09-13 12:00:00" & as.character(storm_name) == "IKE") %>%
  tidyr::spread(quadrant, radii) %>%
  dplyr::select(storm_id, date, latitude, longitude, wind_speed, ne, nw, se, sw)


geom_hurricane <- function(data = NULL, mapping = NULL, scale_radii = 0.5, stat = "identity",
                           position = "identity", na.rm = FALSE, 
                           show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomHurricane, mapping = mapping,  
    data = data, stat = stat, position = position, 
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, scale_radii = scale_radii, ...)
  )
}

# Create a hurricane wind radii plot using geom_hurricane

ggmap::get_map("Louisiana", zoom = 6, maptype = "toner-background") %>%
  ggmap::ggmap(extent = "device") + 
  geom_hurricane(data = storm_observation, ggplot2::aes(x = longitude, y = latitude, r_ne = ne, r_se = se, r_nw = nw, r_sw = sw, fill = wind_speed, color = wind_speed), scale_radii = 1) + 
  ggplot2::scale_color_manual(name = "Wind speed (kts)", 
                     values = c("red", "orange", "yellow")) + 
  ggplot2::scale_fill_manual(name = "Wind speed (kts)", 
                    values = c("red", "orange", "yellow"))