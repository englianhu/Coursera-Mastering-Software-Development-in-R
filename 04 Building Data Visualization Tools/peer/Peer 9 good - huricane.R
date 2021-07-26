# R visualisation course
# huricanes


library(tidyr)
# library(lubridate)
# library(readr)
library(ggmap)
library(grid)

setwd("/Users/cemre/Documents/R")

ext_widths <- c(7, 10, 2, 2, 3, 5, 5, 6, 4, 5, 4, 4, 5, 3, 4, 3, 3, 3,
                       4, 3, 3, 3, 4, 3, 3, 3, 2, 6, 1)
ext_colnames <- c("storm_id", "storm_name", "month", "day",
                         "hour", "year", "latitude", "longitude",
                         "max_wind", "min_pressure", "rad_max_wind",
                         "eye_diameter", "pressure_1", "pressure_2",
                         paste("radius_34", c("ne", "se", "sw", "nw"), sep = "_"),
                         paste("radius_50", c("ne", "se", "sw", "nw"), sep = "_"),
                         paste("radius_64", c("ne", "se", "sw", "nw"), sep = "_"),
                         "storm_type", "distance_to_land", "final")
# ------------------------------------------------------------------------------
# reading & preprocessing

ext_tracks <- readr::read_fwf("ebtrk_atlc_1988_2015.txt",
                              readr::fwf_widths(ext_widths, ext_colnames),
                       na = "-99") %>%
                dplyr::mutate("storm_id" = paste(storm_name, year, sep = "-")) %>%
                tidyr::unite(date, year, month, day, hour) %>%
                dplyr::mutate(date = lubridate::ymd_h(date)) %>%
                dplyr::select(storm_id, date, latitude, longitude, radius_34_ne:radius_64_nw) %>%
                tidyr::gather(key = key, value = value, -storm_id, -date, -latitude, -longitude)%>%
                dplyr::mutate(wind_speed = substr(x = key, start = 8, stop = 9),
                       direction = substr(x = key, start = 11, stop = 12),
                       longitude = -longitude,
                       wind_speed = factor(wind_speed),
                       key = NULL) %>%
                tidyr::spread(direction, value)



#---------------------------------------------------------------------------------------------#
#----------------------------------- Building Geom_hurricane
#---------------------------------------------------------------------------------------------#

# ------------------------------------------------------------------------------
# new stat

#' @param x longitude in degrees
#' @param y latitude in degrees
#' @param r_ne distances in nautical milles, direction ne
#' @param r_se distances in nautical milles, direction se
#' @param r_nw distances in nautical milles, direction nw
#' @param r_sw distances in nautical milles, direction sw


StatHurricane <- ggplot2::ggproto("StatHurricane", ggplot2::Stat,
                        # default_aes = aes(scale_radii = 1),
                        required_aes = c("x", "y", "r_ne", "r_se", "r_nw", "r_sw" ),
                       compute_group = function(data, scales) {

                               if(is.null(data$scale_radii)){
                                       scale_radii = 1
                               }else{
                                       scale_radii = data$scale_radii
                               }



                               contorno <- data.frame(rbind(geosphere::destPoint(p = cbind(data$x,data$y), b=sort(rep(1:90,3)), d = scale_radii*data$r_ne*1852),
                                                            geosphere::destPoint(p = cbind(data$x,data$y), b=sort(rep(91:180,3)),  d = scale_radii*data$r_se*1852),
                                                            geosphere::destPoint(p = cbind(data$x,data$y), b=sort(rep(181:270,3)), d = scale_radii*data$r_sw*1852),
                                                            geosphere::destPoint(p = cbind(data$x,data$y), b=sort(rep(271:360,3)), d = scale_radii*data$r_nw*1852)))

                               names(contorno)          = c("x", "y")

                               contorno

                       }


)


stat_hurricane <- function(mapping = NULL, data = NULL, geom = "polygon",
                           position = "identity", show.legend = NA,
                           outliers = TRUE, inherit.aes = TRUE, ...) {
        ggplot2::layer(
                stat = StatHurricane,
                data = data,
                mapping = mapping,
                geom = geom,
                position = position,
                show.legend = show.legend,
                inherit.aes = inherit.aes,
                params = list(outliers = outliers, ...)
        )
}

# ------------------------------------------------------------------------------
# new geom

#' @param x longitude in degrees
#' @param y latitude in degrees

GeomSimplePolygon <- ggplot2::ggproto("GeomPolygon", ggplot2::Geom,
                             required_aes = c("x", "y"),

                             default_aes = ggplot2::aes(
                                     colour = "grey20", fill = "grey20", size = 0.5,
                                     linetype = 1, alpha = 0.5, scale_radii = 1
                             ),

                             draw_key = ggplot2::draw_key_polygon,

                             draw_group = function(data, panel_params, coord) {

                                     n <- nrow(data)
                                     if (n <= 2) return(grid::nullGrob())

                                     coords <- coord$transform(data, panel_params)

                                     first_row <- coords[1, , drop = FALSE]
                                     # print(first_row)
                                     grid::polygonGrob(
                                             coords$x, coords$y,
                                             default.units = "native",
                                             gp = grid::gpar(
                                                     col = first_row$colour,
                                                     fill = scales::alpha(first_row$fill, first_row$alpha),
                                                     lwd = first_row$size * .pt,
                                                     lty = first_row$linetype
                                             )
                                     )
                             }
)

geom_hurricane <- function(mapping = NULL, data = NULL, stat = "hurricane",
                           position = "identity", na.rm = FALSE, show.legend = NA,
                           inherit.aes = TRUE, ...) {
        layer(
                geom = GeomSimplePolygon, mapping = mapping, data = data, stat = stat,
                position = position, show.legend = show.legend, inherit.aes = inherit.aes,
                params = list(na.rm = na.rm, ...)
        )
}
# ------------------------------------------------------------------------------
# register_google

register_google(key = "AIzaSyCl3F8ebsZjCmWc5EUQkCGz8Ei26mRzt1I")

getOption("ggmap")


# test plot

map_data <- get_map(location = "Louisiana", zoom = 6, maptype = "toner-background")

base_map <- ggmap::ggmap(map_data, extent = "device")

katrina  <- ext_tracks %>%
                dplyr::filter(storm_id %in% "KATRINA-2005",
                              as.character(date) %in% ("2005-08-29 12:00:00"))



katrina_scale_radii_1 <- base_map +
                        geom_hurricane(data = katrina, aes(x = longitude, y = latitude,
                                                           r_ne = ne, r_se = se, r_nw = nw, r_sw = sw,
                                                           fill = wind_speed, color = wind_speed, scale_radii = 1))+
                        scale_color_manual(name = "Wind speed (kts)",
                                           values = c("red", "orange", "yellow")) +
                        scale_fill_manual(name = "Wind speed (kts)",
                                          values = c("red", "orange", "yellow"))+
                        ggtitle("Hurricane Katrina - scale radii = 1")

katrina_scale_radii_0.8 <- base_map +
                        geom_hurricane(data = katrina, aes(x = longitude, y = latitude,
                                                           r_ne = ne, r_se = se, r_nw = nw, r_sw = sw,
                                                           fill = wind_speed, color = wind_speed, scale_radii = 0.8))+
                        scale_color_manual(name = "Wind speed (kts)",
                                           values = c("red", "orange", "yellow")) +
                        scale_fill_manual(name = "Wind speed (kts)",
                                          values = c("red", "orange", "yellow")) +
                        ggtitle("Hurricane Katrina - scale radii = 0.8")

gridExtra::grid.arrange(katrina_scale_radii_1, katrina_scale_radii_0.8, ncol = 2, nrow = 1)

# ------------------------------------------------------------------------------
# hurricane IKE


Ike  <- ext_tracks %>%
        dplyr::filter(storm_id %in% "IKE-2008",
                      as.character(date) %in% ("2008-09-12 18:00:00"))

map_data <- get_map(Ike[,c("longitude", "latitude") ], zoom = 6, maptype = "toner-background")

base_map <- ggmap(map_data, extent = "device")

# scale_radii = 1

IKE_scale_radii_1       <- base_map +
                                geom_hurricane(data = Ike, aes(x = longitude, y = latitude,
                                                                   r_ne = ne, r_se = se, r_nw = nw, r_sw = sw,
                                                                   fill = wind_speed, color = wind_speed, scale_radii = 1))+
                                scale_color_manual(name = "Wind speed (kts)",
                                                   values = c("red", "orange", "yellow")) +
                                scale_fill_manual(name = "Wind speed (kts)",
                                                  values = c("red", "orange", "yellow"))+
                                ggtitle("Hurricane IKE - scale radii = 1")

# scale_radii = 0.8
IKE_scale_radii_0.6     <- base_map +
                                geom_hurricane(data = Ike, aes(x = longitude, y = latitude,
                                                               r_ne = ne, r_se = se, r_nw = nw, r_sw = sw,
                                                               fill = wind_speed, color = wind_speed, scale_radii = 0.6))+
                                scale_color_manual(name = "Wind speed (kts)",
                                                   values = c("red", "orange", "yellow")) +
                                scale_fill_manual(name = "Wind speed (kts)",
                                                  values = c("red", "orange", "yellow")) +
                                ggtitle("Hurricane IKE - scale radii = 0.6")
# ggtitle("Petal and sepal length of iris")

gridExtra::grid.arrange(IKE_scale_radii_1, IKE_scale_radii_0.6, ncol = 2, nrow = 1)
