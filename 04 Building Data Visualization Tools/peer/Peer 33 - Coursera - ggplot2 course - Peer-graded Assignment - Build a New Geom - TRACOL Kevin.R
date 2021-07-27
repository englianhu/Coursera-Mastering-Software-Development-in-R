library(ggplot2)
library(ggmap)
library(dplyr)
library(readr)
library(tidyr)
library(grid)
library(geosphere)
library(roxygen2)
library(devtools)

#API Key: 
register_google(key = "") #Enter key


map_data <- get_map("Louisiana", zoom = 6, maptype = "toner-background")
base_map <- ggmap(map_data, extent = "device")


### Extract the data

# df<-read.fwf(file = "C:/Users/Kevin/Documents/R/Coursera ggplot2 course/ebtrk_atlc_1988_2015.txt",
#          fwf_widths(c(7, 10, 2, 2, 3, 5, 5, 6, 4, 5, 4, 4, 5, 3, 4, 3, 3, 3, 4, 3, 3, 3, 4, 3, 3, 3, 2, 6, 1),  #What i took: c(7,10,7,5,6,6,3,5,4,4,5,4,3,3,3,3,4,3,3,3,4,3,3,3,2,7)
#                  c("storm_id", "storm_name", "month", "day",
#                    "hour", "year", "latitude", "longitude",
#                    "max_wind", "min_pressure", "rad_max_wind",
#                    "eye_diameter", "pressure_1", "pressure_2",
#                    paste("radius_34", c("ne", "se", "sw", "nw"), sep = "_"),
#                    paste("radius_50", c("ne", "se", "sw", "nw"), sep = "_"),
#                    paste("radius_64", c("ne", "se", "sw", "nw"), sep = "_"),
#                    "storm_type", "distance_to_land", "final")
#          ),
#          na = "-99"
# )


ext_tracks_widths <- c(7, 10, 2, 2, 3, 5, 5, 6, 4, 5, 4, 4, 5, 3, 4, 3, 3, 3,
                       4, 3, 3, 3, 4, 3, 3, 3, 2, 6, 1)
ext_tracks_colnames <- c("storm_id_nb", "storm_name", "month", "day",
                         "hour", "year", "latitude", "longitude",
                         "max_wind", "min_pressure", "rad_max_wind",
                         "eye_diameter", "pressure_1", "pressure_2",
                         paste("radius_34", c("ne", "se", "sw", "nw"), sep = "_"),
                         paste("radius_50", c("ne", "se", "sw", "nw"), sep = "_"),
                         paste("radius_64", c("ne", "se", "sw", "nw"), sep = "_"),
                         "storm_type", "distance_to_land", "final")
df <- read_fwf("C:/Users/Kevin/Documents/R/Coursera_ggplot2_course/ebtrk_atlc_1988_2015.txt", 
                       fwf_widths(ext_tracks_widths, ext_tracks_colnames),
                       na = "-99")
#Create ID for storms
df$storm_id <- paste(df$storm_name,df$year,sep="-")
#Invert longitude
df$longitude <- -df$longitude

#Add rows foreach wind speed
df <- pivot_longer(df,cols=radius_34_ne:radius_64_nw,
               names_to = c("wind_speed","direction"), 
               names_pattern="radius_(.*)_(.*)",
               values_to = "radius")
df <- pivot_wider(df,
                  names_from = "direction",
                  values_from ="radius")

#Select Huricanne Ike
df<-df[which(df$storm_name=="IKE"),]



StatPerimeter <- ggproto("StatPerimeter", Stat,
                       compute_group = function(data, scales,scale_radii=500) {
                         ## Compute the line segment endpoints
                         x <- data$x
                         y <- data$y

                         
                         ## Return a new data frame
                         data.frame(
                           rbind(
                                destPoint(c(x,y),b=1:90,d=scale_radii*data$ne),
                                destPoint(c(x,y),b=90:180,d=scale_radii*data$se),
                                destPoint(c(x,y),b=180:270,d=scale_radii*data$sw),
                                destPoint(c(x,y),b=270:360,d=scale_radii*data$nw)
                                )
                           ) %>% rename(x=lon,y=lat)
                       },
                       required_aes = c("x", "y", "ne", "nw", "se", "sw")
)


#' Hurricane statistics
#'
#' Return statistics to plot wind radii of a hurricane 
#'
#' @param mapping	Set of aesthetic mappings created by aes() or aes_(). If specified and inherit.aes = TRUE (the default), it is combined with the default mapping at the top level of the plot. You must supply mapping if there is no plot mapping. 
#' @param data	The data to be displayed in this layer. 
#' @param stat	The statistical transformation to use on the data for this layer, as a string.
#' @param position	Position adjustment, either as a string, or the result of a call to a position adjustment function.
#' @param rule	Either "evenodd" or "winding". If polygons with holes are being drawn (using the subgroup aesthetic) this argument defines how the hole coordinates are interpreted. See the examples in grid::pathGrob() for an explanation.
#' @param ...	Other arguments passed on to layer(). These are often aesthetics, used to set an aesthetic to a fixed value, like colour = "red" or size = 3. They may also be parameters to the paired geom/stat.
#' @param na.rm	If FALSE, the default, missing values are removed with a warning. If TRUE, missing values are silently removed.
#' @param show.legend	logical. Should this layer be included in the legends? NA, the default, includes if any aesthetics are mapped. FALSE never includes, and TRUE always includes. It can also be a named logical vector to finely select the aesthetics to display.
#' @param inherit.aes	If FALSE, overrides the default aesthetics, rather than combining with them. This is most useful for helper functions that define both data and aesthetics and shouldn't inherit behaviour from the default plot specification, e.g. borders().



stat_perimeter <- function(mapping = NULL, data = NULL, geom = "polygon",
                         position = "identity", na.rm = FALSE, 
                         show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatPerimeter, 
    data = data, 
    mapping = mapping, 
    geom = geom, 
    position = position, 
    show.legend = show.legend, 
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


#Test of the stat
# ggplot(data=df[71,],aes(x=latitude,y=longitude, ne=ne, nw=nw, se=se, sw=sw)) +
#   geom_polygon(stat ="perimeter" )

#Create geom for ploting hurricane

#' Hurricane
#'
#' Plot the wind radii of a hurricane
#'
#' @param mapping	Set of aesthetic mappings created by aes() or aes_(). If specified and inherit.aes = TRUE (the default), it is combined with the default mapping at the top level of the plot. You must supply mapping if there is no plot mapping. 
#' @param data	The data to be displayed in this layer. 
#' @param stat	The statistical transformation to use on the data for this layer, as a string.
#' @param position	Position adjustment, either as a string, or the result of a call to a position adjustment function.
#' @param rule	Either "evenodd" or "winding". If polygons with holes are being drawn (using the subgroup aesthetic) this argument defines how the hole coordinates are interpreted. See the examples in grid::pathGrob() for an explanation.
#' @param ...	Other arguments passed on to layer(). These are often aesthetics, used to set an aesthetic to a fixed value, like colour = "red" or size = 3. They may also be parameters to the paired geom/stat.
#' @param na.rm	If FALSE, the default, missing values are removed with a warning. If TRUE, missing values are silently removed.
#' @param show.legend	logical. Should this layer be included in the legends? NA, the default, includes if any aesthetics are mapped. FALSE never includes, and TRUE always includes. It can also be a named logical vector to finely select the aesthetics to display.
#' @param inherit.aes	If FALSE, overrides the default aesthetics, rather than combining with them. This is most useful for helper functions that define both data and aesthetics and shouldn't inherit behaviour from the default plot specification, e.g. borders().

geom_hurricane <- function(mapping = NULL, data = NULL, stat = "perimeter",
                         position = "identity", na.rm = FALSE, 
                         show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomPolygon, mapping = mapping,  
    data = data, stat = stat, position = position, 
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

#Test geom_hurricane
# ggplot(data=df[71,],aes(x=latitude,y=longitude, ne=ne, nw=nw, se=se, sw=sw)) +
#   geom_hurricane()

ggplot(data = df[136:138,]) +
  geom_hurricane(aes(x = longitude, y = latitude,
                     ne = ne, se = se, nw = nw, sw = sw,
                     fill = wind_speed, color = wind_speed)) +
  scale_color_manual(name = "Wind speed (kts)",
                     values = c("red", "orange", "yellow")) +
  scale_fill_manual(name = "Wind speed (kts)",
                    values = c("red", "orange", "yellow")) 


base_map+
  geom_hurricane(data = df[136:138,], aes(x = longitude, y = latitude,
                                     ne = ne, se = se,
                                     nw = nw, sw = sw,
                                     fill = wind_speed,
                                     color = wind_speed)) +
  scale_color_manual(name = "Wind speed (kts)",
                     values = c("red", "orange", "yellow")) +
  scale_fill_manual(name = "Wind speed (kts)",
                    values = c("red", "orange", "yellow"))

