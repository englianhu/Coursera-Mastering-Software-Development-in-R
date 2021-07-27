#### Build a New Geom #####
#
#' Read in and filter hurriance data
#' 
#' This function allows you to convert the hurricane data into an
#' object suitable for using with run_ike or an alternative
#' use of geom_hurricane. A default 
#' 
#' @param data
#' 
#' 
#' @examples 
#' /dontrun {
#' clean_hurricane(data)
#' 
#' }
clean_hurricane <- function(data = "ebtrk_atlc_1988_2015.txt") {
  require(magrittr)
  ext_tracks_widths <- c(7, 10, 2, 2, 3, 5, 5, 6, 4, 5, 4, 4, 5, 3, 4, 3, 3, 3,
                         4, 3, 3, 3, 4, 3, 3, 3, 2, 6, 1)
  ext_tracks_colnames <- c("storm_id", "storm_name", "month", "day",
                           "hour", "year", "latitude", "longitude",
                           "max_wind", "min_pressure", "rad_max_wind",
                           "eye_diameter", "pressure_1", "pressure_2",
                           paste("radius_34", c("ne", "se", "sw", "nw"), sep = "_"),
                           paste("radius_50", c("ne", "se", "sw", "nw"), sep = "_"),
                           paste("radius_64", c("ne", "se", "sw", "nw"), sep = "_"),
                           "storm_type", "distance_to_land", "final")
  
  ext_tracks <- readr::read_fwf(data, 
                         readr::fwf_widths(ext_tracks_widths, ext_tracks_colnames),
                         na = "-99")
  
  data_long <- tidyr::gather(ext_tracks, radius, value, radius_34_ne:radius_64_nw, factor_key=FALSE) %>%
    dplyr::mutate(rad = stringr::str_extract(radius, "[0-9]+")) %>%
    dplyr::mutate(dir = stringr::word(radius, 3, sep = stringr::fixed("_"))) %>%
    dplyr::mutate(date = paste(paste(year,month, day,sep = "-"),
                        paste(hour, ":00", sep = ""))) %>%
    dplyr::mutate(storm_id = paste(storm_name, year, sep = "-")) %>%
    dplyr::select(storm_id, date, latitude, longitude, value, wind_speed = rad, dir) %>%
    tidyr::spread(key = dir, value = value)
  
  ike_data <-  dplyr::filter(data_long, (storm_id == "IKE-2008"))
  
  storm_observation <- ike_data %>%
    dplyr::filter(date == "2008-09-13 06:00") %>%
    dplyr::select(storm_id, date, latitude, longitude, wind_speed, ne, nw, sw, se) %>%
    dplyr::mutate(longitude = -longitude)
  
  storm_observation
}

#' Test geom_hurricane
#' 
#' This function allows you to test the geom_hurricane. The result will displays 
#' the geom_hurricane for hurricane IKE just off the coast of Lousiana.
#' 
#' @param storm_observation Data produced from the clean_hurricane
#' function
#' @param place Character value of a place for get map function
#' 
#' @examples 
#' /dontrun {
#' run_ike(storm_observation, "New Orleans")
#' 
#' }
run_ike <- function(storm_observation, place) {
  # test code for assessment. If the geom is working and the data has been correctly written
  # this function should return a hurricane radii map off the coast of Louisiana.
  
  ggmap::get_map(place, zoom = 6, maptype = "toner-background") %>%
    ggmap::ggmap(extent = "device") +
    geom_hurricane(data = storm_observation,
                   ggplot2::aes(x = longitude, y = latitude, 
                       r_ne = ne, r_se = se, r_nw = nw, r_sw = sw,
                       fill = wind_speed, colour = wind_speed)) + 
    ggplot2::scale_colour_manual(name = "Wind speed (kts)", 
                       values = c("red", "orange", "yellow")) + 
    ggplot2::scale_fill_manual(name = "Wind speed (kts)", 
                      values = c("red", "orange", "yellow"))
}

#' Class GeomHurricane and function geom_hurricane
#' 
#' This is the GeomHurricane class that produces polygon grob for
#' plotting and the corresponding geom_hurricane
#' 
#' @param scale.radii radius scale for storm visual 
#' @param x Longitude coordinate
#' @param y Latitude coordinate
#' @param r_ne Hurricance radius in th NE direction
#' @param r_se Hurricance radius in th SE direction
#' @param r_sw Hurricance radius in th SW direction
#' @param r_nw Hurricance radius in th SW direction
#' @param alpha Set as default to 0.65
#' @param scale_radii Set as default to 0.8
#' @param colour Set as default to wind_speed
#' @param fill Set as default to wind_speed
#' @export
geom_hurricane <- function(mapping = NULL, data = NULL, stat = "identity",
                           position = "identity", na.rm = FALSE,
                           show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomHurricane, mapping = mapping,
    data = data, stat = stat, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

#' GeomHurricane Class
#'
#'
#' @export
GeomHurricane <- ggplot2::ggproto("GeomHurricane", ggplot2::Geom,
                         required_aes = c("x", "y", "r_ne", "r_se", "r_sw", "r_nw"),
                         default_aes = ggplot2::aes(alpha = 0.65, scale_radii = 0.8,
                           fill = wind_speed, colour = wind_speed
                         ),
                         draw_key = ggplot2::draw_key_polygon,
                         draw_group = function(data, panel_scales, coord) {
                           start_data <- data
                           # Extract base parameters for plotting
                           colour <- data$colour
                           fill <- data$fill
                           alpha <- data$alpha
                           
                           # multiply the scale  radii by 1852m (nautical miles to metres)
                           scale_factor <- data$scale_radii * 1852
                           
                           # Get points function using destPoint
                           get_points <- function(x,y,start, finish, quad, scale) {
                             geosphere::destPoint(c(x,y), seq(start, finish, 0.1), data[,quad]*scale)
                           }
                           
                           # column heading in a vector
                           quadrants <- c("r_ne", "r_se", "r_sw", "r_nw")
                           geo_points <- data.frame()
                           
                           # loop to produce all data points in a dataframe: geo_points
                           for(i in 1:4) {
                             a <- get_points(data$x, data$y, (i - 1)* 90, i*90, quadrants[i],
                             scale_factor)
                             geo_points <- rbind(geo_points, a)
                           }
                           names(geo_points) <- c("x", "y")
                           
                           coords <- coord$transform(geo_points, panel_scales)
                           
                           grid::polygonGrob(
                             coords$x, coords$y,
                             default.units = "native",
                             gp = grid::gpar(
                               col = colour,
                               fill = scales::alpha(fill, alpha)
                             )
                           )
                         }
)
