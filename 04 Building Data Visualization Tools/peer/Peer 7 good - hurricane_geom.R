## Coursera - Building Data Visualization Tools - Week 4
## M.A. Sondergard - June, 2021

## Load needed libraries
library(tidyverse)
library(geosphere)
library(lubridate)
library(ggmap)


#' @title          Read_ext_tracks
#' @description    Read in ext_tracks file
#'
#' @param file     Filename (character data)
#' @param widths   Vector of column widths
#' @param colnames Character vector of column names
#'
#' @inheritParams readr::read_fwf
#'
#' @return        data.frame
#'
#' @importFrom readr read_fwf
#'
#' @export
#'

## Data formatting - code from Coursera
ext_tracks_widths <- c(
                       7, 10, 2, 2, 3, 5, 5, 6, 4, 5, 4, 4, 5, 3, 4, 3, 3, 3,
                       4, 3, 3, 3, 4, 3, 3, 3, 2, 6, 1
                       )

ext_tracks_colnames <- c(
                         "storm_id", "storm_name", "month", "day",
                         "hour", "year", "latitude", "longitude",
                         "max_wind", "min_pressure", "rad_max_wind",
                         "eye_diameter", "pressure_1", "pressure_2",
                          paste("radius_34", c("ne", "se", "sw", "nw"), sep = "_"),
                          paste("radius_50", c("ne", "se", "sw", "nw"), sep = "_"),
                          paste("radius_64", c("ne", "se", "sw", "nw"), sep = "_"),
                          "storm_type", "distance_to_land", "final"
                          )

ext_tracks <- read_fwf("ebtrk_atlc_1988_2015.txt",
                        fwf_widths(ext_tracks_widths, ext_tracks_colnames),
                        na = "-99"
                        )

## Make longitude into a negative value
ext_tracks$longitude <- -(ext_tracks$longitude)


## ----------------------------------------------------------------------------
#' @title tidy_storm_tracks
#'
#' @description Tidy data formatting of hurricane data
#'
#' @param ext_tracks Output from the read_ext_tracks function
#' #'
#' @return  Tidy data.frame (tibble)
#'
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom lubridate ymd_h
#' @importFrom stringr str_sub
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr pivot_wider

#' @export

# Combine date and time into a single variable with lubridate package

ext_tracks$date <- lubridate::ymd_h(paste(ext_tracks$year,
                                    ext_tracks$month,
                                    ext_tracks$day,
                                    ext_tracks$hour,
                                    sep = ""
                                    ))

## Create "storm_id" field that combines storm name and year
ext_tracks$storm_id <- paste(ext_tracks$storm_name, "-", 
                             ext_tracks$year,
                             sep = ""
                             )

## Convert the data into long format with separate rows for each wind speed for
## distance radii

tidy_storm_tracks <- ext_tracks %>%
                     dplyr::select(
                                   "storm_id", "date", "latitude", "longitude",
                                    starts_with("radius_34"),
                                    starts_with("radius_50"),
                                    starts_with("radius_64")
                     ) %>%
                     tidyr::pivot_longer(
                                         cols = starts_with("radius_"),
                                         names_to = "direction"
                     ) %>%
                     dplyr::mutate(wind_speed = stringr::str_sub(direction, 8, 9)
                     ) %>%
                     dplyr::mutate(quadrant = stringr::str_sub(direction, 11, 12)
                     ) %>%
                     dplyr::select(
                                   "storm_id", "date", "longitude", "latitude",
                                   "value", "quadrant", "wind_speed"
                     ) %>%
                     dplyr::rename(wind_radius = value)


## Running pivot_wider gives the same results in the Coursera example
## "wind_radius" is the distance in nautical miles from eye of the hurricane that
## the wind of a given strength was measured

tidy_storm_tracks_wide <- tidy_storm_tracks %>%
                          tidyr::pivot_wider(
                                             names_from = quadrant,
                                             values_from = wind_radius
                          )

## Subset for Hurricane Ike
## 2008-09-13 at 12:00 hrs chosen - long format

hurricane_ike_data_long <- tidy_storm_tracks %>%
                           dplyr::filter(
                                          storm_id == "IKE-2008",
                                          date == ymd_hms("2008-09-13 12:00:00")
                                         )

## Subset for Hurricane Katrina for comparison with Coursera example

hurricane_katrina_data_wide <- tidy_storm_tracks_wide %>%
                               dplyr::filter(
                                             storm_id == "KATRINA-2005",
                                             date == ymd_hms("2005-08-29 12:00:00")
                                            )

## Preferred format for subsequent processing (long data)

hurricane_katrina_data_long <- tidy_storm_tracks %>%
                               dplyr::filter(
                                             storm_id == "KATRINA-2005",
                                             date == ymd_hms("2005-08-29 12:00:00")
                                            )

## ----------------------------------------------------------------------------
## 
#' @title        hurricane_geocode
#' @description  generation of the data points (long, lat) to be plotted
#
#' @param scale_radii Implement to scale the radius size.
#'
#' @import ggplot2
#' @import grid
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_rows
#' @importFrom dplyr filter
#' @importFrom dplyr rename
#' @importFrom tibble tibble
#' @importFrom geosphere destPoint
#' 
#' @example
#' \dontrun{
#' data0 <- hurricane_geocode()  # see below
#' }
#' 
#' @export

hurricane_geocode <- function(storm_data, 
                              x = "longitude", 
                              y = "latitude", 
                              r = "wind_radius",
                              quadrant = "quadrant",
                              wind_speed = "wind_speed",
                              arcRes = 1,
                              scale_radii = 1.0) 
    {
    # Renaming arguments
    storm_data <- dplyr::rename(storm_data,
                                x = longitude,
                                y = latitude,
                                r = wind_radius,
                                quadrant = quadrant,
                                wind_speed = wind_speed) 

    
  # Merging data needed for geodesic inputs with each record
    arc_data <- tibble::tibble(quadrant = c("ne", "se", "sw", "nw"),
                               start_angle = c(0, 90, 180, 270),
                               end_angle = c(90, 180, 270, 360))
      
    merged_data <- merge(storm_data, arc_data, by = "quadrant")
  
  # Points for polygon for each speed and the speed above it
  speeds <- unique(merged_data$wind_speed)
  
  for (s in 1:length(speeds)) {
    arc_data <- dplyr::filter(
                              merged_data,
                              wind_speed %in% c(speeds[s], speeds[s + 1]))
    
    for (a in 1:nrow(arc_data)) 
      {
      if (arc_data$wind_speed[a] == speeds[s]) 
        {
        arc_data$radtype[a] <- "outer"
      } else {
        arc_data$radtype[a] <- "inner"
      }
      
      ## Defining the long and lat for the eye of the hurricane
      ad_p <- c(arc_data$x[a], arc_data$y[a])
      
      ## Converting nautical miles to metres for destPoint() function
      ad_d <- arc_data$r[a] * 1852 * scale_radii
      
      # Sequence of angles from start to end, 1 degree at a time
      ad_s <- unique(c(seq(arc_data$start_angle[a],
                           arc_data$end_angle[a],
                           by = arcRes),
                           arc_data$end_angle[a]))
      
      ## Geocode arcs for plotting
      ## p - longitude and latitude of points in degrees; can be vector of 2 numbers
      ## b - bearing (direction) in degrees (numeric)
      ## d - numeric, distance in meters
      points <- data.frame(geosphere::destPoint(
                                           p = ad_p, # centre
                                           d = ad_d, # bearing
                                           b = ad_s  # sequence of angles
                                           ))
      
      points$angle <- ad_s
      points[, names(arc_data)] <- arc_data[a, ]
      points$wind_speed <- speeds[s]
      if (s == 1 && a == 1) {
        out <- points
        } else {
        out <- dplyr::bind_rows(out, points)
        }
    }
  }
  
  # Ordering and removal collection of points at 0 (radius == zero)
  
  out$quadrant <- factor(out$quadrant, levels = c("ne", "se", "sw", "nw"))
  
  ## Arrange changes the order of rows
  out <- dplyr::arrange(out, wind_speed, radtype, quadrant, angle)
  out <- dplyr::filter(out, r > 0)
 
  # Renaming coordinates
  
  out <- dplyr::rename(out, eye_lon = x, eye_lat = y)
  out <- dplyr::rename(out, x = lon, y = lat)
  
  # cleaning extra variables
  
  out$angle <- NULL
  out$quadrant <- as.character(out$quadrant)
  
  return(out)
}

## ----------------------------------------------------------------------------
#' @title       GeomHurricane
#' @description ggproto object to display the hurricane radii plots
#
#' @param scale_radii Implement to scale the radius size.
#'
#' @import ggplot2
#' @import grid
#' @importFrom dplyr select
#' @importFrom dplyr left_join
#'
#' @export

## gproto() function is used to construct a new class corresponding
## to the new geom

GeomHurricane <- ggplot2::ggproto("GeomHurricane", GeomPolygon,
                          required_aes = c("x",
                                           "y",
                                           "r",
                                           "quadrant",
                                           "wind_speed",
                                           "scale_radii"),
                          
                          default_aes = aes(fill = "red",
                                            colour = "red",
                                            size = 0.5,
                                            linetype = 1,
                                            alpha = 0.5
                                           ),
    
    # Data transformation for draw panel
    
    draw_panel = function(self, data, panel_scales, coord)
                 {
                 extravars <- dplyr::select(data, -x, -y, -r)

                 
        # Geocoded points generation from hurricane_geocode() function above
    
        data0 <- hurricane_geocode(storm_data,
                                   x = "x",
                                   y = "y",
                                   r = "r",
                                   quadrant = "quadrant",
                                   wind_speed = "wind_speed",
                                   arcRes = 1,
                                   scale_radii = 0.5)

    # Merging aesthetics mapping
    
    data0 <- dplyr::select(data0, x, y, wind_speed, quadrant)
    extravars$quadrant <- factor(extravars$quadrant, levels = c("ne","se","sw","nw"))
    extravars$wind_speed <- as.factor(extravars$wind_speed)
    
    data0$quadrant <- factor(data0$quadrant, levels = c("ne", "se", "sw", "nw"))
    data0$wind_speed <- as.factor(data0$wind_speed)
    data0 <- dplyr::left_join(data0, extravars, by = c("wind_speed", "quadrant"))
            
    data0$group <- data0$wind_speed

    ggplot2:::ggname("geom_polygon", 
                      GeomPolygon$draw_panel(data0, 
                                             panel_scales,
                                             coord))
    }
    )

## ----------------------------------------------------------------------------
## Function that builds a layer based on geom specification
## Takes the geom definition and the coordinates data together
#' 
#' @title geom_hurricane  

geom_hurricane <- function(mapping = NULL,
                           data = NULL,
                           stat = "identity",
                           position = "identity",
                           na.rm = FALSE,
                           show.legend = NA,
                           inherit.aes = TRUE, ...) 
    {
    ggplot2::layer(
             geom = GeomHurricane,
             mapping = mapping,
             data = data,
             stat = stat,
             position = position,
             show.legend = show.legend,
             inherit.aes = inherit.aes,
             params = list(na.rm = na.rm, ...))
  
    }

##-----------------------------------------------------------------------------
## Test script

library(ggmap)
library(ggplot2)

register_google(key = "AIzaSyB-L6OQXiwEvSnOMhZ22itRNF-7vXDl8Q8")

map <- get_map("Louisiana", zoom = 6, maptype = "toner-background")

## Note that geom_hurricane expects un-transformed data (long format)

full_size <- ggmap(map) +
  
  geom_hurricane(data = hurricane_ike_data_long,
                 mapping = aes(x = longitude,
                               y = latitude,
                               r = wind_radius,
                               wind_speed = wind_speed,
                               quadrant = quadrant,
                               fill = as.factor(wind_speed),
                               color = as.factor(wind_speed),
                               scale_radii = 1.0)) +
  
  scale_color_manual(name = "Wind speed (kts)",
                     values = c("red",
                                "orange",
                                "yellow")) +
  
  scale_fill_manual(name = "Wind speed (kts)",
                    values = c("red",
                               "orange",
                               "yellow")) +
  
  ggtitle("Hurricane Ike - scale_radii = 1.00")

full_size


half_size <- ggmap(map) +
  
  geom_hurricane(data = hurricane_ike_data_long,
                 mapping = aes(x = longitude,
                               y = latitude,
                               r = wind_radius,
                               wind_speed = wind_speed,
                               quadrant = quadrant,
                               scale_radii = 0.5,
                               fill = as.factor(wind_speed),
                               color = as.factor(wind_speed))) +
  
  scale_color_manual(name = "Wind speed (kts)",
                     values = c("red",
                                "orange",
                                "yellow")) +
  
  scale_fill_manual(name = "Wind speed (kts)",
                    values = c("red",
                               "orange",
                               "yellow")) +
  
  ggtitle("Hurricane Ike - scale_radii = 0.50")

half_size

final <- gridExtra::grid.arrange(full_size, half_size, ncol = 1, nrow = 2)

ggsave("hurricane_ike_plots.pdf", plot = final)
