#' 
#' Hurricane Assignment
#' 
#' Following information is used to complete an assignment in Building
#'     Vizualiation Tools in R 
#'     
#' @import readr
#' @import reshape
#' @import stringr
#' @import geosphere
#' @import dplyr
#' @import tidyr
#' @import purr
#' @import ggplot2
#' @import ggmap  
#' 
#' Cleaning Data 
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

ext_tracks <- read_fwf("ebtrk_atlc_1988_2015.txt", 
                       fwf_widths(ext_tracks_widths, ext_tracks_colnames),
                       na = "-99")

ext_tracks$storm_id <- paste(ext_tracks$storm_name, "-", ext_tracks$year, sep = "")
ext_tracks$date <- paste(ext_tracks$year, "-", ext_tracks$month, "-", ext_tracks$day, sep = "")
ext_tracks$date <- paste(ext_tracks$date, paste(ext_tracks$hour, ":00", sep = ""))

ext_tracks$longitude <- ext_tracks$longitude*-1

ext_tracks <- ext_tracks[c("storm_id", "date", "latitude", "longitude", "radius_34_ne" ,   
                           "radius_34_se" ,    "radius_34_sw" ,    "radius_34_nw" ,   
                           "radius_50_ne"  ,   "radius_50_se"  ,   "radius_50_sw" ,   
                           "radius_50_nw"  ,   "radius_64_ne" ,    "radius_64_se" ,   
                           "radius_64_sw" ,    "radius_64_nw")]


ext_tracks <- as.data.frame(ext_tracks)
ext_tracks <- melt(ext_tracks, id = c("storm_id"   ,  "date"     ,    "latitude"  ,   "longitude"  ))
library(stringr)
string <- str_split_fixed(ext_tracks$variable, "_", 3)
ext_tracks <- cbind(ext_tracks, string)
ext_tracks$wind_speed <- ext_tracks$`2`

ext_tracks_ne <- ext_tracks[which(ext_tracks$`3` == "ne"),]
ext_tracks_ne$ne <- ifelse(ext_tracks_ne$`3` == "ne", as.numeric(paste(ext_tracks_ne$value)), NA)
ext_tracks_ne <- ext_tracks_ne[c("storm_id" ,  "date"     ,  "latitude"  , "longitude" , "wind_speed", "ne")]

ext_tracks_nw <- ext_tracks[which(ext_tracks$`3` == "nw"),]
ext_tracks_nw$nw <- ifelse(ext_tracks_nw$`3` == "nw", as.numeric(paste(ext_tracks_nw$value)), NA)
ext_tracks_nw <- ext_tracks_nw[c("storm_id" ,  "date"     ,  "latitude"  , "longitude" , "wind_speed", "nw")]


ext_tracks_se <- ext_tracks[which(ext_tracks$`3` == "se"),]
ext_tracks_se$se <- ifelse(ext_tracks_se$`3` == "se", as.numeric(paste(ext_tracks_se$value)), NA)
ext_tracks_se <- ext_tracks_se[c("storm_id" ,  "date"     ,  "latitude"  , "longitude" , "wind_speed", "se")]

ext_tracks_sw <- ext_tracks[which(ext_tracks$`3` == "sw"),]
ext_tracks_sw$sw <- ifelse(ext_tracks_sw$`3` == "sw", as.numeric(paste(ext_tracks_sw$value)), NA)
ext_tracks_sw <- ext_tracks_sw[c("storm_id" ,  "date"     ,  "latitude"  , "longitude" , "wind_speed", "sw")]


ext_tracks <- merge(ext_tracks_ne, ext_tracks_nw, by = c("storm_id", "date", "latitude", "longitude", "wind_speed"))
ext_tracks <- merge(ext_tracks, ext_tracks_se, by = c("storm_id", "date", "latitude", "longitude", "wind_speed"))
ext_tracks <- merge(ext_tracks, ext_tracks_sw, by = c("storm_id", "date", "latitude", "longitude", "wind_speed"))

#' select hurricane ike 2008 2008-09-12 18:00:00

hurricaneIKE <- ext_tracks[which(ext_tracks$storm_id == "IKE-2008" & ext_tracks$date == "2008-09-12 18:00"),]

#' Creating the Stat for Hurricane to create quadrant functions
#' 
#' @param data
#' @param sclaes
#' @param scale_radii

StatHurricane <-
  ggplot2::ggproto("StatHurricane", ggplot2::Stat,
                   compute_group = function(data, scales, scale_radii) {
                     lon <- data$x[1]
                     lat <- data$y[1]
                     new_arc <- function(direction, radius){
                       my_arc <- geosphere::destPoint(c(lon, lat),
                                                      dplyr::case_when(direction == "r_ne" ~ 0:90,
                                                                       direction == "r_se" ~ 90:180,
                                                                       direction == "r_sw" ~ 180:270,
                                                                       direction == "r_nw" ~ 270:360),
                                                      radius*1852*scale_radii) %>%
                         rbind(data.frame(lon=lon, lat=lat)) %>% 
                         dplyr::rename(x=lon, y=lat)
                     }
                     
                     df <-
                       data %>%
                       dplyr::select(r_ne, r_nw, r_se, r_sw) %>%
                       tidyr::gather(direction, radius) 
                     
                     grid <-
                       purrr::pmap(list(df$direction, df$radius), new_arc) %>%
                       dplyr::bind_rows()
                     
                     return(grid)
                   },
                   required_aes = c("x", "y","r_ne", "r_se", "r_nw", "r_sw")
  )

#' create stat_hurricane
#'  
stat_hurricane <- function(mapping = NULL, data = NULL, geom = "polygon", 
                           position = "identity", na.rm = FALSE, show.legend = NA, 
                           inherit.aes = TRUE, scale_radii = 1, ...) {
  ggplot2::layer(
    stat = StatHurricane, data = data, mapping = mapping, geom = geom, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(scale_radii = scale_radii, na.rm = na.rm, ...)
  )
}
#'
#' use ggproto to create geomHurricane which will be used to create geom_hurricane
#'
#' @param StatHurricane
#' 
geomHurricane <-
  ggplot2::ggproto("GeomPolygon", ggplot2::GeomPolygon,
                   default_aes = ggplot2::aes(colour = "green", 
                                              fill = NA, linetype = 1, 
                                              size = 1, alpha = 0.6)
  )

geom_hurricane <- function(mapping = NULL, data = NULL,
                           position = "identity", na.rm = FALSE, 
                           show.legend = NA, inherit.aes = TRUE, 
                           scale_radii = 1.0, ...) {
  
  ggplot2::layer(stat = StatHurricane, geom = geomHurricane,
                 data = data, mapping = mapping, position = position,
                 show.legend = show.legend, inherit.aes = inherit.aes,
                 params = list(scale_radii = scale_radii, na.rm = na.rm, ...)
  )
}

#' Test geom_hurricane 
ggplot(data = hurricaneIKE) +
  geom_hurricane(aes(x = longitude, y = latitude,
                     r_ne = ne, r_se = se, r_nw = nw, r_sw = sw,
                     fill = wind_speed, color = wind_speed)) +
  scale_color_manual(name = "Wind speed (kts)",
                     values = c("red", "orange", "yellow")) +
  scale_fill_manual(name = "Wind speed (kts)",
                    values = c("red", "orange", "yellow"))


#' Place on map 
#' 

map_data <- get_map(location = "Louisiana", zoom = 6, maptype = "toner-background", source = "stamen")
base_map <- ggmap::ggmap(map_data, extent = "device")

#' Scale_radii = 1
base_map +
  geom_hurricane(data = ike_observation,
                 aes(x = longitude, y = latitude,
                     r_ne = ne, r_se = se, r_nw = nw, r_sw = sw,
                     fill = wind_speed, color = wind_speed), scale_radii = 1) +
  scale_color_manual(name = "Wind speed (kts)",
                     values = c("red", "orange", "yellow")) +
  scale_fill_manual(name = "Wind speed (kts)",
                    values = c("red", "orange", "yellow"))

#' Use scale_radii 0.5
base_map +
  geom_hurricane(data = ike_observation,
                 aes(x = longitude, y = latitude,
                     r_ne = ne, r_se = se, r_nw = nw, r_sw = sw,
                     fill = wind_speed, color = wind_speed), scale_radii = 0.5) +
  scale_color_manual(name = "Wind speed (kts)",
                     values = c("red", "orange", "yellow")) +
  scale_fill_manual(name = "Wind speed (kts)",
                    values = c("red", "orange", "yellow"))
