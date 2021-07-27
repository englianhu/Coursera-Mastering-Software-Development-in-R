# Coursera: Building Data Vizualisation Tools
# Tomás Murray
# 2021/01/07
# Week 4 Assignment: Build a New Geom

# ------------------------------------------------------------------------------
# Set working directory
# ------------------------------------------------------------------------------

setwd("C:/Users/MurrayT/Documents/Work/Coursera/4_DataVisualisation/w4assign")
getwd()

# ------------------------------------------------------------------------------
# Libraries
# ------------------------------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(ggmap)
library(geosphere)
library(grid)
library(gridExtra)
library(png)

# ------------------------------------------------------------------------------
# Load and clean data
# ------------------------------------------------------------------------------

ext_tracks_widths <- c(7, 10, 2, 2, 3, 5, 5, 6, 4, 5, 4, 4, 5, 3, 4, 3, 3, 3,
                       4, 3, 3, 3, 4, 3, 3, 3, 2, 6, 1)

ext_tracks_colnames <- c("storm_origid", "storm_name", "month", "day",
                         "hour", "year", "latitude", "longitude",
                         "max_wind", "min_pressure", "rad_max_wind",
                         "eye_diameter", "pressure_1", "pressure_2",
                         paste("radius_34", c("ne", "se", "sw", "nw"), sep = "_"),
                         paste("radius_50", c("ne", "se", "sw", "nw"), sep = "_"),
                         paste("radius_64", c("ne", "se", "sw", "nw"), sep = "_"),
                         "storm_type", "distance_to_land", "final")

ext_tracks <- read_fwf("./data/ebtrk_atlc_1988_2015.txt", 
                       fwf_widths(ext_tracks_widths, ext_tracks_colnames),
                       na = "-99")

sum(is.na(ext_tracks$longitude)) # Check for NA in longitude 

ext_clean <- ext_tracks %>% 
  unite("storm_id", c("storm_name","year"), sep = "-", remove = FALSE) %>% 
  mutate(longitude = longitude*-1) %>%
  mutate(hour = format(strptime(paste0(hour, "0000"), format = "%H%M%S"), format = "%H:%M:%S")) %>%
  unite("date", c("year","month","day"),sep = "-", remove = FALSE) %>% 
  unite("date", c("date","hour"), sep = " ", remove = FALSE) %>% 
  gather("rad_dir", "value", radius_34_ne:radius_64_nw) %>% 
  mutate("dir" = str_sub(rad_dir, -2, -1)) %>% 
  mutate("wind_speed" = str_sub(rad_dir, -5, -4)) %>% 
  select(-"rad_dir") %>% 
  spread("dir", "value") %>% 
  select("storm_id", "date", "latitude", "longitude", "wind_speed", "ne", "nw", "se", "sw")

glimpse(head(ext_clean))

# ------------------------------------------------------------------------------
# Subset to Hurricane Ike and a single observation time
# ------------------------------------------------------------------------------

ike <- ext_clean %>% 
  filter(grepl("IKE", storm_id)) %>% 
  filter(date == "2008-09-11 12:00:00")

# For comparison with assignment example

katrina <- ext_clean %>% 
  filter(grepl("KATRINA", storm_id)) %>% 
  filter(date == "2005-08-29 12:00:00")

# ------------------------------------------------------------------------------
# Create geom_hurricane function to test
# ------------------------------------------------------------------------------

# Inputs of the geom data x y r_ne r_se r_nw r_sw fill color

geom_beta <- function(data = data,
                      x = longitude,
                      y = latitude,
                      r_ne = ne,
                      r_se = se,
                      r_nw = nw,
                      r_sw = sw,
                      fill = wind_speed,
                      color = wind_speed,
                      scale_radii = 1) {

df_beta <- list() # create empty list for dataframes from ne, se, sw and nw CHECK

centre <- cbind(data$longitude, data$latitude) # store hurricane point of origin

r_cardinal <- cbind(data$ne * scale_radii, 
                    data$se * scale_radii, 
                    data$sw * scale_radii, 
                    data$nw * scale_radii) # store speed in data frame scaled by scale_radii

# Loop to create the cardinal data set for each wind speed 34, 50 and 64 knot
for(i in 1:4) {
  # Generate the polygon points using destPoint
  for (j in 1:nrow(data)) {             # For each row of data
  destPoint(centre[j, ],                # Hurricane origin
    b = ((i-1)*90):(90*i),              # Generate each quadrant of 360 degrees
    d = r_cardinal[j, i] * 1852) %>%    # Radius converting nm to m
    as_tibble() %>%                     # Convert to tibble
    mutate(j = j) %>%            # Add column j for filtering when plotting
    bind_rows(df_beta) -> df_beta
  }
}

# Fix extent of Stamen map (this could be extended to scale with radii)

sac_borders <- c(bottom  = centre[1,2] - 8, 
                 top     = centre[1,2] + 8,
                 left    = centre[1,1] - 8,
                 right   = centre[1,1] + 8)

# Load a Stamen map

get_stamenmap(sac_borders, zoom = 5, maptype = "toner-background") %>% 

# Plot with ggmap
ggmap(extent = "device") +
  
  geom_polygon(data = (df_beta %>% filter(j == 1)), # 64 kts
              aes(x = lon, 
                  y = lat,
                  fill = data$wind_speed[1],
                  color = data$wind_speed[1]),
                  alpha = 0.5) +
  
  geom_polygon(data = (df_beta %>% filter(j == 2)), # 50 kts
               aes(x = lon, 
                   y = lat,
                   fill = data$wind_speed[2],
                   color = data$wind_speed[2]),
               alpha = 0.5) +
  
  geom_polygon(data = (df_beta %>% filter(j == 3)), # 34 kts
               aes(x = lon, 
                   y = lat,
                   fill = data$wind_speed[3],
                   color = data$wind_speed[3]),
               alpha = 0.5)
  
}

# Add additional specifications as per assignment instructions

geom_beta(data = katrina, scale_radii = 1.0) +
  scale_color_manual(name = "Wind speed (kts)",
                     values = c("red", "orange", "yellow")) +
  scale_fill_manual(name = "Wind speed (kts)",
                      values = c("red", "orange", "yellow"))

# ------------------------------------------------------------------------------
# Create new geomHurricane geom class
# ------------------------------------------------------------------------------

#' Create a new ggplot2 geom
#'
#' This is a function that generates a new geom class for hurricane data
#' from \url{http://rammb.cira.colostate.edu/research/tropical_cyclones/tc_extended_best_track_dataset/}
#'
#' @return A ggplot2 geom class
#' 
#' @importFrom ggplot2 ggproto  
#' @importFrom dplyr as_tibble mutate select rename
#' @importFrom geosphere destPoint
#' @importFrom grid polygonGrob gpar
#'
#' @export
geomHurricane <- ggplot2::ggproto("geomHurricane",
                                 Geom,
                                 required_aes = c("x",      # longitude
                                                  "y",      # latitude
                                                  "r_ne",   # northeast radius
                                                  "r_se",   # southeast radius
                                                  "r_sw",   # southwest radius
                                                  "r_nw"),  # northwest radius
                                 
                                 default_aes = aes(color = "black",    # line colour
                                                   fill = "black",     # standard fill colour
                                                   linetype = 0,       # no line
                                                   alpha = 0.5,        # transparancey
                                                   scale_radii = 1.0), # default value
                                 
                                 draw_key = draw_key_polygon,
                                 
                                 draw_group = function(data,
                                                       panel_scales,
                                                       coord) {
                                   
                 # Creating a data frame
                 df_hurricane <- dplyr::as_tibble()
                 center       <- dplyr::as_tibble()
                                   
                 # Adding new columns to data
                 data %>% dplyr::mutate(fill = fill,     # Creating columns to assign variables
                                        colour = colour) #
                                   
                 # Center of the hurricane
                 data %>% dplyr::select(lon = x,           # Longitude
                                        lat = y) -> center # Latitude
                                   
                 # Calculating the area/radius
                 data %>% dplyr::select(r_ne,       # Subset
                                        r_se,       # 
                                        r_sw,       #
                                        r_nw) %>%   #
                                     
                 dplyr::mutate(r_ne = data$scale_radii * r_ne * 1852, # Convert nk to m
                               r_se = data$scale_radii * r_se * 1852, # 
                               r_sw = data$scale_radii * r_sw * 1852, # 
                               r_nw = data$scale_radii * r_nw * 1852) -> radius
                                   
                 # Loop for each quadrant
                 for (i in 1:4)
                 {
                       # Loop for each wind speed zone
                       for (j in 1:nrow(data))
                       {
                                 # Generating the points
                                 geosphere::destPoint(c(x = center[j,1],        # Centre of hurricane
                                                        y = center[j,2]),       # 
                                                        b = ((i-1)*90):(90*i),  # Loop through each quadrant
                                                        d = radius[j,i]) %>%    # Radius
                                         
                                             rbind(c(x = center[j,1],       # Longitude
                                                     y = center[j,2])) %>%  # Latitude
                                           
                                             rbind(df_hurricane) -> df_hurricane # Add to empty df_hurricane list
                                     }
                                     
                 # Data Manipulation
                 df_hurricane %>% 
                                       
                  dplyr::as_tibble() %>% # Converting to tibble
                                       
                  dplyr::rename(x = lon,      # Rename as ouput of destPoint() function is lon and lat
                                y = lat) %>%  # 
                                       
                  coord$transform(panel_scales) -> quadrant_points # Cleaned data for plotting
                  }
                                   
                  # Plot the polygon
                  grid::polygonGrob(x = quadrant_points$x,   # Longitude
                                    y = quadrant_points$y,   # Latitude
                                    default.units = "native",
                                    gp = grid::gpar(col = data$colour,  # Use input line colour
                                                    fill = data$fill,   # Use input fill colour
                                                    alpha = data$alpha, # Default value
                                                    lty = 1,            # Default value
                                                    scale_radii = data$scale_radii))   # scale_radii       
                  }
)

# ------------------------------------------------------------------------------
# Create corresponding geom function
# ------------------------------------------------------------------------------

#' Build a ggplot layer with a new geom
#'
#' This function builds a ggplot layer based on the new geom specification
#'
#' @return A ggplot2 layer instance
#' 
#' @importFrom ggplot2 layer 
#' 
#' @param mapping Aesthetic mappings
#' @param data Hurricane data to include in layer
#' @param stat Statistical transformation of data for this layer as a string
#' @param position Adjust position on plot
#' @param na.rm FALSE, missing values removed with warning; TRUE, missing valuessilently removed
#' @param show.legend TRUE includes legend; FALSE or NA doesn't
#' @param inherit.aes FALSE overrides default aesthetics
#' @param geom A ggplot2 geom class
#' 
#' @export

geom_hurricane <- function(mapping = NULL,
                           data = NULL,
                           stat = "identity",
                           position = "identity",
                           na.rm = FALSE,
                           show.legend = NA,
                           inherit.aes = TRUE, ...){
  
  ggplot2::layer(geom = geomHurricane,
                 mapping = mapping,
                 data = data,
                 stat = stat,
                 position = position,
                 show.legend = show.legend,
                 inherit.aes = inherit.aes,
                 params = list(na.rm = na.rm,...)
                )
}                             
  
# ------------------------------------------------------------------------------
# Create plot using new geom class
# ------------------------------------------------------------------------------

# Fix extent of Stamen map (I don't have a Google API key)

centre <- cbind(ike$longitude, ike$latitude)
sac_borders <- c(bottom  = centre[1,2] - 8, 
                 top     = centre[1,2] + 8,
                 left    = centre[1,1] - 8,
                 right   = centre[1,1] + 8)

# Load a Stamen map and assign as basemap

get_stamenmap(sac_borders, zoom = 5, maptype = "toner-background") %>% 
  ggmap(extent = "device") -> basemap

# Plot using new geomHurricane class

basemap + geom_hurricane(data = ike,
                 aes(x = longitude,
                     y = latitude,
                     r_ne = ne,
                     r_se = se,
                     r_nw = nw,
                     r_sw = sw,
                     fill = wind_speed,
                     color = wind_speed)) +
  
  scale_color_manual(name = "Wind speed (kts)",
                     values = c("red",
                                "orange",
                                "yellow")) +
  
  scale_fill_manual(name = "Wind speed (kts)",
                    values = c("red",
                               "orange",
                               "yellow"))

# ------------------------------------------------------------------------------
# Produce .png file for submission
# ------------------------------------------------------------------------------

png(filename = "geomAssignment.png", width = 800, res = 100)

# Scale_radii = 1.0

mp1 <- basemap + geom_hurricane(data = ike,
                                aes(x = longitude,
                                    y = latitude,
                                    r_ne = ne,
                                    r_se = se,
                                    r_nw = nw,
                                    r_sw = sw,
                                    fill = wind_speed,
                                    color = wind_speed)) +
  
  scale_color_manual(name = "Wind speed (kts)",
                     values = c("red",
                                "orange",
                                "yellow")) +
  
  scale_fill_manual(name = "Wind speed (kts)",
                    values = c("red",
                               "orange",
                               "yellow")) +
  
  ggtitle(label = "scale_radii = 1.0")

# Scale_radii = 1.5

mp2 <- basemap + geom_hurricane(data = ike,
                                aes(x = longitude,
                                    y = latitude,
                                    r_ne = ne,
                                    r_se = se,
                                    r_nw = nw,
                                    r_sw = sw,
                                    fill = wind_speed,
                                    color = wind_speed,
                                    scale_radii = 1.5)) +
  
  scale_color_manual(name = "Wind speed (kts)",
                     values = c("red",
                                "orange",
                                "yellow")) +
  
  scale_fill_manual(name = "Wind speed (kts)",
                    values = c("red",
                               "orange",
                               "yellow")) +
  
  ggtitle(label = "scale_radii = 1.5")

grid.arrange(mp1, mp2, ncol = 2, top = textGrob("2008-09-11 12:00:00 - Hurricane Ike", gp  = gpar(fontsize = 20)))

dev.off()
