rm(list=ls())
require(readr)
require(dplyr)
require(reshape2)
require(grid)
require(geosphere)
require(scales)
require(ggmap)

#---------------- Build geom_hurricane -------------------#
#' Make hurricane plot
#'
#' This function makes a wind rose plot for hurricane wind speed.
#' 
#'
#' @param  data Dataset name.
#' geom_hurricane() understands the following aesthetics:
#' @param x Longitude as numeric vector
#' @param Y Latitude as numeric vector
#' @param r_ne Northeast wind distance.
#' @param r_se Southeast wind distance.
#' @param r_sw Southwest wind distance.
#' @param r_nw Northwest wind distance.
#' @param fill Fill color
#' @param color Line color
#' @param scale_radii Maximum wind scaling variable (optional)
#'
#' @return Wind rose plot.
#'
#' @examples
#' map_data <- ggmap::get_map("Louisiana", zoom = 6, maptype = "toner-background")
#' base_map <- ggmap::ggmap(map_data, extent = "device")
#' base_map +
#'     geom_hurricane(data = storm_observation, aes(x = longitude, y = latitude, r_ne = ne, 
#'     r_se = se, r_sw = sw, r_nw = nw,
#'     fill = wind_speed, color = wind_speed),
#'    scale_radii = 0.5) +
#'    ggplot2::scale_color_manual(name = "Wind speed (kts)",
#'                     values = c("red", "orange", "yellow")) +
#'    ggplot2::scale_fill_manual(name = "Wind speed (kts)",
#'                    values = c("red", "orange", "yellow"))
#'                    
#' @importFrom readr read_fwf fwf_widths
#' @importFrom dplyr mutate %>% filter
#' @importFrom reshape2 melt dcast
#' @importFrom grid polygonGrob
#' @importFrom geosphere destPoint
#' @importFrom scales rescale
#' @importFrom ggmap get_map ggmap


StatHurricane <- ggplot2::ggproto("StatHurricane", Stat,
                                  compute_group = function(data, scales, scale_radii = 1) {
                                    
                                    ## Make data with latitudes and longitudes for each orientation
                                    
                                    # Middle of the plot
                                    start_point <- c(unique(data$x), unique(data$y)) 
                                    
                                    # Convert distance destpoints uses meters & data has nautical miles
                                    # + add scaling
                                    distance <- 1000*1.852*scale_radii
                                    
                                    # North east
                                    data_ne <- geosphere::destPoint(start_point, b = 1:90,
                                                                    d = data$r_ne*distance)
                                    
                                    # South east
                                    data_se <- geosphere::destPoint(start_point, b = 91:180,
                                                                    d = data$r_se*distance)
                                    
                                    # south west
                                    data_sw <- geosphere::destPoint(start_point,  b= 181:270,
                                                                    d = data$r_sw*distance)
                                    
                                    # North west
                                    data_nw <- geosphere::destPoint(start_point, b = 271:360,
                                                                    d = data$r_nw*distance)
                                    
                                    # Combine
                                    data_all <- rbind(data_ne, data_se, data_sw, data_nw)
                                    data_all <- as.data.frame(data_all)
                                    data_all
                                    
                                  },
                                  required_aes = c("x", "y", "r_ne","r_se", "r_sw", "r_nw"),
                                  default_aes = c(scale_radii = 1)
)

stat_hurricane <- function(mapping = NULL, data = NULL, geom = "hurricane",
                           position = "identity", na.rm = FALSE, 
                           show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatHurricane, 
    data = data, 
    mapping = mapping, 
    geom = geom, 
    position = position, 
    show.legend = show.legend, 
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

draw_panel_function = function(data, panel_scales, coord) {
  ## Transform the data first
  coords <- coord$transform(data, panel_scales)
  coords$x = scales::rescale(coords$lon, from = panel_scales$x.range)
  coords$y = scales::rescale(coords$lat, from = panel_scales$y.range)
  
  grid::polygonGrob(
    id = coords$group,
    x = coords$x,
    y = coords$y,
    gp = gpar(
      col = unique(coords$colour),
      fill = unique(coords$fill),
      lwd = 1,
      lty = 1,
      alpha = 0.7))
}

GeomHurricane <- ggplot2::ggproto("GeomHurricane", Geom,
                                  required_aes = c("x", "y"), 
                                  default_aes = aes(colour = "NA",
                                                    fill = "grey20", size = 0.5,
                                                    linetype = 1,
                                                    alpha = NA, subgroup = NULL),
                                  draw_key = draw_key_polygon,
                                  draw_panel = draw_panel_function
)


geom_hurricane <- function(mapping = NULL, data = NULL, stat = "hurricane", 
                           position = "identity", na.rm = FALSE, 
                           show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomHurricane,   
    position = position, 
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
###################################################################################################
###################################################################################################
#---------------- Make data -------------------#
path <- "my_path_to_data"
setwd(path)

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

ext_tracks <- readr::read_fwf("ebtrk_atlc_1988_2015.txt", 
                              readr::fwf_widths(ext_tracks_widths, ext_tracks_colnames),
                       na = "-99")

# Make data
# measurement variables ("radius_34_ne", etc. will be used for melting later)
meas_vars <- grep("radius", names(ext_tracks), value = TRUE)

ext_tracks <- ext_tracks %>% 
  dplyr::mutate(storm_id = paste(toTitleCase(tolower(storm_name)), year, sep = "-"),
         date = ISOdatetime(year, month, day, hour, 0, 0), longitude = - longitude) %>%
  reshape2::melt(id.vars = c("storm_id", "date", "latitude", "longitude"),
                 measure.vars = meas_vars, value.name = "value") %>%
  dplyr::mutate(wind_speed = gsub("[[:alpha:]]|_","", variable),
                orientation := substr(variable, 11, 12)) %>%
  reshape2::dcast(storm_id + date  + latitude + longitude + wind_speed ~ orientation,
                  value.var = "value")

  
#---------------- Pick storm observation -------------------#
storm_observation <- ext_tracks %>%
  dplyr::filter(storm_id == "Ike-2008" & date == "2008-09-13 06:00:00")

map_data <- ggmap::get_map("Louisiana", zoom = 6, maptype = "toner-background")
base_map <- ggmap::ggmap(map_data, extent = "device")

#---------------- Plot without scaling -------------------#
base_map +
  geom_hurricane(data = storm_observation, aes(x = longitude, y = latitude, r_ne = ne,
                                               r_se = se, r_sw = sw, r_nw = nw,
                                               fill = wind_speed, color = wind_speed)) +
  ggplot2::scale_color_manual(name = "Wind speed (kts)",
                     values = c("red", "orange", "yellow")) +
  ggplot2::scale_fill_manual(name = "Wind speed (kts)",
                    values = c("red", "orange", "yellow"))

#---------------- Plot with scaling to 0.5 -------------------#
base_map +
  geom_hurricane(data = storm_observation, aes(x = longitude, y = latitude, r_ne = ne,
                                               r_se = se, r_sw = sw, r_nw = nw,
                                               fill = wind_speed, color = wind_speed)
                 , scale_radii = 0.5) +
  ggplot2::scale_color_manual(name = "Wind speed (kts)",
                     values = c("red", "orange", "yellow")) +
  ggplot2::scale_fill_manual(name = "Wind speed (kts)",
                    values = c("red", "orange", "yellow"))
