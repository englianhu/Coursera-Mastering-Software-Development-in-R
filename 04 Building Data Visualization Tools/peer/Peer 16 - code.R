###--- R-script for the exercise "Build new Geom" ---###

#--- The first 81 lines of this script are for a data preparation
#--- The implementation of the GeomHurricane starts on line 82

# Attach libraries
library(readr)
library(dplyr)
library(lubridate)
library(tidyr)
library(stringr)
library(geosphere)
library(ggplot2)
library(ggmap)
library(gridExtra)

# Load data
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
                       fwf_widths(ext_tracks_widths, ext_tracks_colnames),
                       na = "-99")

# Tidy and subset data
dat <- ext_tracks %>%
  dplyr::select(storm_name, month, day, hour, year, latitude, longitude, starts_with("radius_")) %>%
  dplyr::mutate(storm_name = paste(storm_name, year, sep = "-")) %>%
  dplyr::rename("storm_id" = storm_name) %>%
  dplyr::mutate(date = paste(year, month, day, hour, sep = "/")) %>%
  dplyr::mutate(date = ymd_h(date)) %>%
  dplyr::select(storm_id, date, latitude, longitude, starts_with("radius_"))

# Focus on IKE-2008 on 2008-09-11-12
dat <- dat %>%
  dplyr::filter(grepl("IKE-2008", storm_id)) %>%
  dplyr::filter(date == ymd_h("2008-09-11-12"))

# Some data manipulation
dat_ne <- dat %>%
  dplyr::select(storm_id, date, latitude, longitude, ends_with("ne")) %>%
  tidyr::gather(key = "wind_speed", value = "ne", -storm_id, -latitude, -longitude, -date) %>%
  dplyr::mutate(wind_speed = wind_speed %>% str_extract("[0-9]+"))

dat_se <- dat %>%
  dplyr::select(storm_id, ends_with("se")) %>%
  tidyr::gather(key = "wind_speed", value = "se", -storm_id) %>%
  dplyr::mutate(wind_speed = wind_speed %>% str_extract("[0-9]+"))

dat_sw <- dat %>%
  dplyr::select(storm_id, ends_with("sw")) %>%
  tidyr::gather(key = "wind_speed", value = "sw", -storm_id) %>%
  dplyr::mutate(wind_speed = wind_speed %>% str_extract("[0-9]+"))

dat_nw <- dat %>%
  dplyr::select(storm_id, ends_with("nw")) %>%
  tidyr::gather(key = "wind_speed", value = "nw", -storm_id) %>%
  dplyr::mutate(wind_speed = wind_speed %>% str_extract("[0-9]+"))


dat_ike <- dat_ne %>%
  dplyr::left_join(dat_se, by = c("storm_id", "wind_speed")) %>%
  dplyr::left_join(dat_sw, by = c("storm_id", "wind_speed")) %>%
  dplyr::left_join(dat_nw, by = c("storm_id", "wind_speed"))

# wind_speed should be a double and
# expressed in meters instead of nautical miles (is converted by multiplying with 1852)
# Longitude should be a negative number
dat_ike <- dat_ike %>%
  dplyr::mutate(wind_speed = as.double(wind_speed)) %>%
  dplyr::mutate(ne = 1852 * ne, se = 1852 * se, sw = 1852 * sw, nw = 1852 * nw ) %>%
  dplyr::mutate(longitude = -longitude)

#----- A function draw_group_new for geom_hurricane

draw_group_new <- function(data, panel_scales, coord) {
#' A draw_group - function for the GeomHurricane
#'
#' This function overwrites a draw_group - function of a Geom class
#'
#' @param data a data frame containing one column for each aesthetic specified
#' @param panel_scales  a list containing information about the x and y scales for the current panel
#' @param coord an object that describes the coordinate system of your plot
#' @return a grid grob -object
#' @import dplyr
#' @import geosphere


  # Center of the hurricane
  center <- data %>% dplyr::select(lon = x, lat = y)

  # radius
  radius <- data %>%
    dplyr::select(r_ne, r_se, r_sw, r_nw) %>%
    dplyr::mutate(r_ne = data$scale_radii * r_ne ,
                  r_se = data$scale_radii * r_se ,
                  r_sw = data$scale_radii * r_sw ,
                  r_nw = data$scale_radii * r_nw )

  # Initialize tibble
  df_hurricane <- dplyr::as_tibble()

  # Loop to create the for quadrants
  for (i in 1:4)
  {
    # For each quadrant: Loop to create the 34, 50 and 64 wind speed areas
    for (j in 1:nrow(data))
    {
      # Generating the points
      df_hurricane <- geosphere::destPoint(
        c(x = center[j,1], y = center[j,2]),
        b = ((i-1)*90):(90*i),
        d = radius[j,i]
      ) %>%
        rbind(c(x = center[j,1], y = center[j,2])) %>%
        rbind(df_hurricane)
    }

    # Data Manipulation
    quadrant_points <-
      df_hurricane %>%
      dplyr::as_tibble() %>%
      dplyr::rename(x = lon, y = lat) %>%
      coord$transform(panel_scales)
  }

  # Plot the polygon
  grid::polygonGrob(x = quadrant_points$x,
                    y = quadrant_points$y,
                    default.units = "native",
                    gp = grid::gpar(col = data$colour,
                                    fill = data$fill,
                                    alpha = data$alpha,
                                    lty = 1,
                                    scale_radii = data$scale_radii))
}


#----- Class GeomHurricane
GeomHurricane <- ggplot2::ggproto("GeomHurricane", Geom,
                                  required_aes = c("x",
                                                   "y",
                                                   "r_ne",
                                                   "r_se",
                                                   "r_sw",
                                                   "r_nw"),

                                  default_aes = ggplot2::aes(
                                    colour = "black",  # Line color
                                    fill = "black",  # Standard Fill color
                                    linetype = 0,        # No line
                                    alpha = 1.0,     # Transparency
                                    scale_radii = 1.0),     # Default value (no reduction)

                                  draw_key = draw_key_polygon,
                                  draw_group = draw_group_new

#' A ggproto-function for the class Geom_hurricane
)

#----- geom_hurricane function

geom_hurricane <- function(mapping = NULL,
                           data = NULL,
                           stat = "identity",
                           position = "identity",
                           na.rm = FALSE,
                           show.legend = NA,
                           inherit.aes = TRUE, ...){

#' A function that will build a layer based on GeomHurricane
#'
#' @param geom
#' @param mapping
#' @param data
#' @param stat
#' @param position
#' @param show.legend
#' @param inherit.aes
#' @param params


  ggplot2::layer(geom = GeomHurricane,
                 mapping = mapping,
                 data = data,
                 stat = stat,
                 position = position,
                 show.legend = show.legend,
                 inherit.aes = inherit.aes,
                 params = list(na.rm = na.rm,...)
  )
}



#----- required output
dat_ike %>%
  ggplot2::ggplot() +
  geom_hurricane(aes(x = longitude,
                     y = latitude,
                     r_ne = ne,
                     r_se = se,
                     r_nw = nw,
                     r_sw = sw,
                     fill = factor(wind_speed),
                     color = factor(wind_speed),
                     scale_radii = 1)) +
  scale_color_manual(name = "Wind speed (kts)",
                     values = c("red", "orange", "yellow")) +
  scale_fill_manual(name = "Wind speed (kts)",
                    values = c("red", "orange", "yellow"))


# Registration is needed to use google maps
register_google(key = "AIzaSyDCH0pW1w5bib77pfSdq86AJ6oRYguowkk")

myMap <- get_map(location = c(-88.9,25.8), # c(longitude,latitude)
        zoom = 5,
        maptype = "toner-background")


ggmap(myMap, extent = "device") +
geom_hurricane(data= dat_ike, aes(x = longitude,
                     y = latitude,
                     r_ne = ne,
                     r_se = se,
                     r_nw = nw,
                     r_sw = sw,
                     fill = factor(wind_speed),
                     color = factor(wind_speed),
                     scale_radii = 1)) +
  scale_color_manual(name = "Wind speed (kts)",
                     values = c("red", "orange", "yellow")) +
  scale_fill_manual(name = "Wind speed (kts)",
                    values = c("red", "orange", "yellow"))

# Two maps with different scale_radiis
map1 <- ggmap(myMap, extent = "device") +
  geom_hurricane(data= dat_ike, aes(x = longitude,
                                    y = latitude,
                                    r_ne = ne,
                                    r_se = se,
                                    r_nw = nw,
                                    r_sw = sw,
                                    fill = factor(wind_speed),
                                    color = factor(wind_speed),
                                    scale_radii = 1)) +
  scale_color_manual(name = "Wind speed (kts)",
                     values = c("red", "orange", "yellow")) +
  scale_fill_manual(name = "Wind speed (kts)",
                    values = c("red", "orange", "yellow"))

map2 <- ggmap(myMap, extent = "device") +
  geom_hurricane(data= dat_ike, aes(x = longitude,
                                    y = latitude,
                                    r_ne = ne,
                                    r_se = se,
                                    r_nw = nw,
                                    r_sw = sw,
                                    fill = factor(wind_speed),
                                    color = factor(wind_speed),
                                    scale_radii = 0.5)) +
  scale_color_manual(name = "Wind speed (kts)",
                     values = c("red", "orange", "yellow")) +
  scale_fill_manual(name = "Wind speed (kts)",
                    values = c("red", "orange", "yellow"))

# Plotting
gridExtra::grid.arrange(map1, map2, ncol = 2)
