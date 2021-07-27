library(tidyr)
library(dplyr)
library(stringr)
library(lubridate)
library(readr)


setwd("Documents/r")
#'
#' @description helper function to read data from hurricane dataset. Specifies
#' columns width and their names from hurricane text file.
ext_tracks_widths <- c(7, 10, 2, 2, 3, 5, 5, 6, 4, 5, 4, 4, 5, 3, 4, 3, 3, 3,
                       4, 3, 3, 3, 4, 3, 3, 3, 2, 6, 1)
#'
ext_tracks_colnames <- c("storm_id", "storm_name", "month", "day",
                         "hour", "year", "latitude", "longitude",
                         "max_wind", "min_pressure", "rad_max_wind",
                         "eye_diameter", "pressure_1", "pressure_2",
                         paste("radius_34", c("ne", "se", "sw", "nw"), sep = "_"),
                         paste("radius_50", c("ne", "se", "sw", "nw"), sep = "_"),
                         paste("radius_64", c("ne", "se", "sw", "nw"), sep = "_"),
                         "storm_type", "distance_to_land", "final")
#'
ext_tracks <- readr::read_fwf("data/ebtrk_atlc_1988_2015.txt", 
                       fwf_widths(ext_tracks_widths, ext_tracks_colnames),
                       na = "-99")
#'
#' @description Get data frame with hurricanes data, and format to  a specified 
#' format.
single_track <- ext_tracks %>%
  # storm_id field as hurricane_name+year_occurrence
  dplyr::mutate(storm_id = paste(str_to_title(storm_name),year,sep = '-')) %>%
  # date grabs data and formats as yyyy-mm-dd hh:mm
  dplyr::mutate(date = ymd_h(paste(ext_tracks$year, ext_tracks$month, 
                                   ext_tracks$day, ext_tracks$hour, sep = '-'))) %>%
  # add hemisphere correction
  dplyr::mutate(longitude = -longitude) %>%
  #' filtered Katrina hurricane as an example: katrina 2005,at 12:00pm
  dplyr::filter(storm_id =="Katrina-2005", date == as.POSIXct("2005-08-29 12:00", tz="UTC")) %>%
  #' data to be used as input to our Geom
  dplyr::select(storm_id, date, longitude, latitude, starts_with("radius"))
#'
#' from wind radii info, generate three rows for each wind speed (34,50,64)
katrina <- single_track %>%
  tidyr::gather(c1, m1, radius_34_ne:radius_64_nw ) %>%
  # update upon condition c1
  dplyr::mutate(wind_speed= case_when(
    grepl("34",c1) ~ "34",
    grepl("50",c1) ~ "50",
    grepl("64",c1) ~ "64"
    )) %>%
  # assign quadrants
  dplyr::mutate(c1= case_when(
    grepl("ne",c1) ~ "ne",
    grepl("se",c1) ~ "se",
    grepl("nw",c1) ~ "nw",
    grepl("sw",c1) ~ "sw"
    )) %>%
  # wrap with updated data
  tidyr::spread(c1, m1)

##
library(grid)
library(ggplot2)
library(geosphere)

#' Print "GeomHurricane"
#'
#' @description Generate a new ggproto class object.
#'
#' @param _class Class name to assign to the object
#' 
#' @param _inherit ggproto object to inherit from
#' 
#' @param required_aes required aesthetics for the geom object. Required parameters:
#' for the geom object
#'  -x: longitude value in degrees
#'  -y: latitude value in degrees
#'  -r_ne: first quadrant with wind radii inforamtion
#'  -r_se: second quadrant with wind radii inforamtion
#'  -r_sw: third quadrant with wind radii inforamtion
#'  -r_nw: fourth quadrant with wind radii inforamtion
#'  -alpha: modifies object transparency
#'  -factor_scale: controls output object size
#' 
#' @param default_aes aesthetics default values. Default values:
#'  -fill: fill color for wind radii are "red","orange","yellow"; from outer to
#'  inner radii chart
#'  -colour: color list for wind radii
#' 
#' @param draw_key function to draw the key in the legend
#' 
#' @param draw_panel function that returns a grid grob object to be plotted. This
#' function takes three arguments:
#'  -data: data.frame with one column per aesthetic
#'  -panel_scales: list which contains (x,y) scales for current panel
#'  -coord: object that describes the coordinate system of the current plot 
#'
#' @return This function returns a polygon grob object.
#'
#' @importFrom geosphere destPoint
#' @importFrom grid polygonGrob, gpar
#' @importFrom scales alpha
GeomHurricane <- ggproto("GeomPolygon", Geom,
                         required_aes = c("x","y","r_ne","r_se","r_nw","r_sw"),
                         default_aes = aes(fill=c("red","orange","yellow"), 
                                           colour=NA,alpha=0.5,factor_scale=1),
                         draw_key = draw_key_point,
                         draw_panel = function(data, panel_scales, coord) {
                           ## Transform the data first
                           coords <- coord$transform(data, panel_scales)
                           ## Generate datapoints for first quadrant
                           wind_ne = geosphere::destPoint(cbind(coords$x,coords$y),
                                              b=c(1:90), 
                                              d=c(coords$r_ne*50*coords$factor_scale))
                           ## Generate datapoints for second quadrant
                           wind_se = geosphere::destPoint(cbind(coords$x,coords$y),
                                              b=c(91:180), 
                                              d=c(coords$r_se*50*coords$factor_scale))
                           ## Generate datapoints for third quadrant
                           wind_sw = geosphere::destPoint(cbind(coords$x,coords$y),
                                               b=c(181:270), 
                                               d=c(coords$r_sw*50*coords$factor_scale))
                           ## Generate datapoints for quadrant quadrant
                           wind_nw = geosphere::destPoint(cbind(coords$x,coords$y),
                                               b=c(271:360), 
                                               d=c(coords$r_nw*50*coords$factor_scale))
                           ## bind 4 quadrants
                           circle <- rbind(wind_ne,wind_se,wind_sw,wind_nw)
                           ##
                           grid::polygonGrob(
                             x = circle[,'lon'],
                             y = circle[,'lat'],
                             default.units = "native",
                             id = rep(1:3,nrow(circle)/3),
                             gp = grid::gpar(
                               # modify colour transparency
                               fill=scales::alpha(coords$fill,coords$alpha),
                               col=scales::alpha(coords$colour,coords$alpha))
                             )
                        })

#' Print "geom_hurricane"
#'
#' @description This function creates the layer based on the geomHurricane.
#'
#' @param years A vector that contains POSIXlt date time format
#'
#' @return This function returns the layer to be plotted according to the
#' GeomHurricane.
#'
#' @examples
#' \dontrun{
#' base_map +
#' geom_hurricane(data=katrina,
#'          aes(x=longitude,
#'              y=latitude,
#'              r_ne=ne, 
#'              r_se=se, 
#'              r_nw=nw, 
#'              r_sw=sw))
#' }
#'
#' @importFrom ggplot layer
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

library(OpenStreetMap)
#'
#' @description Get Louisiana map represented by 9 tiles, and specified by
#' upper-left corner and bottom-right coordinates (longitude, latitude). Specified
#' map type 'esri' among different options.
#' 
base_map <- OpenStreetMap::openmap(
  c(lat= 30.453409130203596,lon= -91.4007568359375),
  c(lat= 29.22889003019423,   lon= -88.582763671875),
  minNumTiles=9,type="esri")
#'
#' @description Project new map generated as WGS84 coordinates
#' 
base_map <- OpenStreetMap::openproj(
  base_map, 
  projection = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
#'
#' @description pipe that plots openmap using ggplot2, then adds geom_hurricane
#' created and finally modifies legend options
#' 
OpenStreetMap::autoplot.OpenStreetMap(base_map) +
  geom_hurricane(
    data=katrina, 
    aes(x=longitude,y=latitude,r_ne=ne, r_se=se, r_nw=nw, r_sw=sw, colour=wind_speed)) +
  scale_color_manual(name = "Wind speed (kts)", values = c("red", "orange", "yellow")) +
  scale_fill_manual(name = "Wind speed (kts)", values = c("red", "orange", "yellow")) +
  xlab("Longitude") + 
  ylab("Latitude")
