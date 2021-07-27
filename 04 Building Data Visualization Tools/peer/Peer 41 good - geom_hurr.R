#-------------------------------------Start of Script-----------------------------------------#
#----------------------------Building a new geom...geom_hurricane-----------------------------#
#---------------------------------------------------------------------------------------------#
#--------------------------------Data Input and Cleaning--------------------------------------#
#---------------------------------------------------------------------------------------------#

#' Function to import the hurricanes dataset into the system.
#'
#' @param filename The name of the file to be imported.
#'
#' @return The imported dataset.
#' 
#' @importFrom readr read_fwf
#' @importFrom readr fwf_widths
#' 
#' @export
#'
#' @examples input_Data("ebtrk_atlc_1988_2015.txt")
#' 
input_Data <- function(filename) {
  
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
  
  ext_tracks <- readr::read_fwf(filename, 
                                readr::fwf_widths(ext_tracks_widths, ext_tracks_colnames),
                                na = "-99")
  return(ext_tracks)
  
}
#---------------------------------------------------------------------------------------------#
#' Function to clean and manipulate the raw imported data on hurricanes.
#'
#' @param data The raw data file - an object of class data.frame of tibble or tbl_df.
#'
#' @return Cleaned data file with the required features.
#' 
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom tidyr gather
#' @importFrom tidyr separate
#' 
#' @export
#'
#' @examples tidy_Data(ext_tracks)
#' 
tidy_Data <- function(data) {
  ## Tidying the entire dataset ##
  tidy_storm <- data %>%
    dplyr::select("storm_name", "day", "month", "year", "hour", "latitude", "longitude",
                  "radius_34_ne", "radius_34_nw", "radius_34_sw", "radius_34_se",
                  "radius_50_ne", "radius_50_nw", "radius_50_sw", "radius_50_se",
                  "radius_64_ne", "radius_64_nw", "radius_64_sw", "radius_64_se") %>%
    dplyr::mutate(storm_name = paste(storm_name, year, sep = "-")) %>%
    dplyr::mutate(date = paste(year, month, day, sep ="-"), time = paste(hour, "00", "00", sep = ":"),
                  date = paste(date, time, sep = " ")) %>%
    dplyr::select("storm_name", "date", latitude:radius_64_se) %>%
    dplyr::mutate(longitude = dplyr::if_else(longitude < 180, -longitude,
                                             dplyr::if_else(longitude >= 180, longitude, NA_real_))) %>%
    dplyr::mutate(`34` = paste(radius_34_ne, radius_34_nw, radius_34_sw, radius_34_se, sep = "_"), 
                  `50` = paste(radius_50_ne, radius_50_nw, radius_50_sw, radius_50_se, sep = "_"),
                  `64` = paste(radius_64_ne, radius_64_nw, radius_64_sw, radius_64_se, sep = "_")) %>%
    tidyr::gather(`34`, `50`, `64`, key = "wind_speed", value = "direction") %>%
    tidyr::separate(col = "direction", into = c("ne", "nw", "sw", "se"), sep = "_") %>%
    dplyr::select("storm_name", "date", "latitude", "longitude", "wind_speed", "ne", "nw", "sw", "se")
  return(tidy_storm)
}
#----------------------------------------------------------------------------------------------#
#' Function to extract data for one hurricane for one observation time
#'
#' @param data The cleaned data file of hurricanes of class data.frame or tibble or tbl_df.
#' @param hurricane An object of class character or string.
#'                  The name of the hurricane whose info is needed. The format is <Hurricane name>-<Year>
#'                  Example:- for hurricane Ike in 2008, write "IKE-2008".
#' @param obs_time An object of class character or string.
#'                 The observation time for which the hurricane data is desired in the format <Date> <Time>.
#'                 Example:- for some hurricane on Sep 12th, 2008, at 6:00 AM, write "2008-09-12 06:00:00"
#'
#' @return An object of class data.frame, tibble or tbl_df.
#' 
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr across
#' @importFrom dplyr all_of
#' 
#' @export
#' 
#' @note The time of storm follows a 24 Hour convention.
#' @note Include a space between the date and time of storm.
#' @note Include the year of occurrence of storm with its name.
#' @note Use block letters to write the name of the storm. 
#'
#' @examples filtered_data(tidystorm, "KATRINA-2005", "2005-06-15 12:00:00")
#' @examples filtered_data(tidystorm, "IKE-2008", "2008-09-12 18:00:00")
#' 
filtered_data <- function(data, hurricane, obs_time) { # function to filter the data for one hurricane
  new_data <- data %>%                                 # per one observation time.
    dplyr::filter(storm_name == hurricane & date == obs_time) %>%
    dplyr::mutate(
      dplyr::across(dplyr::all_of("ne"), as.numeric),
      dplyr::across(dplyr::all_of("se"), as.numeric),
      dplyr::across(dplyr::all_of("nw"), as.numeric),
      dplyr::across(dplyr::all_of("sw"), as.numeric))
  return(new_data)
}

#-----------------------------------------------------------------------------------------------#
#-----------------------------------draw panel function-----------------------------------------# 
#-----------------------------------------------------------------------------------------------#
# I purposely wrote this panel function explicitly because of its good length.
#' The panel function to plot the data.
#'
#' @param data The data to be plotted; ideally an object of class data.frame, tibble or tbl_df
#' @param panel_scales A list containing info about the x and y scales of the current pannel
#' @param coord An object that decribes the coordinate system of the plot.
#'
#' @return A grid grob that will be plotted.
#' 
#' @importFrom dplyr as_tibble()
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom geosphere destpoint
#' 
#' 
#' @export
#'
#' @examples
#' 
draw_panel_function <- function(data, panel_scales, coord) {
  # Creating a data frame
  df_hurricane <- dplyr::as_tibble()
  center       <- dplyr::as_tibble()
  
  # Adding new columns to de data
  data %>% dplyr::mutate(fill = fill,     # Creating columns to assign variables
                         colour = colour) #
  
  # Center of the hurricane
  data %>% dplyr::select(lon = x,           # longitude
                         lat = y) -> center # latitude
  
  # Calculating the area/radius
  data %>% dplyr::select(r_ne,       # 
                         r_se,       # Subsetting
                         r_sw,       #
                         r_nw) %>%   #
    
    dplyr::mutate(r_ne = data$scale_radii * r_ne * 1852, # Converting nautical knots 
                  r_se = data$scale_radii * r_se * 1852, # to meters : knots * 1852
                  r_sw = data$scale_radii * r_sw * 1852, # scale_radii : scale variable
                  r_nw = data$scale_radii * r_nw * 1852) -> radius
  
  # Loop to create the for quadrants (columns)
  for (i in 1:4)
  {
    # For each quadrant: Loop to create the 34, 50 and 64 knot areas (rows)
    for (j in 1:nrow(data))
    {
      # Generating the points
      geosphere::destPoint(c(x = center[j,1],        # Center of the "circle"
                             y = center[j,2]),       # 
                           b = ((i-1)*90):(90*i),  # 360 degrees (a complete circle)
                           d = radius[j,i]) %>%    # radius
        
        rbind(c(x = center[j,1],       # Longitude
                y = center[j,2])) %>%  # Latitude
        
        rbind(df_hurricane) -> df_hurricane # Output: Will be stacked over iteration
    }
  }
}


#-------------------------------------------------------------------------------------------#
#--------------------Defining the Geom class using ggproto----------------------------------#
#-------------------------------------------------------------------------------------------#
GeomHurricane <- ggplot2::ggproto("GeomHurricane",
                                  ggplot2::Geom,
                                  required_aes = c("x",  # x = longitude
                                                   "y",     # y = latitude
                                                   "r_ne",  # Northeast radius
                                                   "r_se",  # Southeast radius
                                                   "r_sw",  # Southwest radius
                                                   "r_nw"), # Northwest radius
                                  
                                  default_aes = ggplot2::aes(colour  = "black",  # Line color
                                                             fill        = "black",  # Standard Fill color
                                                             linetype    = 0,        # No line
                                                             alpha       = 0.65,     # Transparency
                                                             scale_radii = 1.0),     # Default value (no reduction)
                                  
                                  draw_key = ggplot2::draw_key_polygon,
                                  
                                  draw_group = draw_panel_function # Function is written explicitly
                                  )

# Default functions
geom_hurricane <- function(mapping = NULL,
                           data = NULL,
                           stat = "identity",
                           position = "identity",
                           na.rm = FALSE,
                           show.legend = NA,
                           inherit.aes = TRUE, ...){
  
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

#-------------------------------Data Accumulation-----------------------------------#

# Dataset 1  with observation time of 13th September 2008 at 06:00 AM #
ike <- input_Data("ebtrk_atlc_1988_2015.txt") %>% 
  tidy_Data %>% 
  filtered_data(hurricane = "IKE-2008", obs_time = "2008-09-13 06:00:00")
#------------------------------Observation table------------------------------------#
# storm_name            date      latitude  longitude  wind_speed   ne  nw  sw  se
#1	IKE-2008	 2008-09-13 06:00:00	 29.1	   -94.6	       34	      225	125	125	200
#2	IKE-2008	 2008-09-13 06:00:00	 29.1	   -94.6	       50	      150	75	80	160
#3	IKE-2008	 2008-09-13 06:00:00	 29.1	   -94.6	       64	      110	45	55	90
# Dataset 2  with observation time of 12th September 2008 at 06:00 PM #
ike2 <- input_Data("ebtrk_atlc_1988_2015.txt") %>% 
  tidy_Data %>% 
  filtered_data(hurricane = "IKE-2008", obs_time = "2008-09-12 18:00:00")
#------------------------------Observation table------------------------------------#
# storm_name            date      latitude  longitude  wind_speed   ne  nw  sw  se
#1	IKE-2008	 2008-09-12 18:00:00	 27.5	   -93.2	       34	      240	175	150	205
#2	IKE-2008	 2008-09-12 18:00:00	 27.5	   -93.2	       50	      150	105	90	150
#3	IKE-2008	 2008-09-12 18:00:00	 27.5	   -93.2	       64	      105	75	60	90
#-----------------------------------------------------------------------------------#

# Loading ggmap package
library(ggmap)
#-----------------------------------------------------------------------------------#
# API Key
register_google(key = "AIzaSyCTh2X9pEEiGAa7lV5Qik5dqxcaAiabJwM")
#-----------------------------------------------------------------------------------#
#------------------------------Plots for Dataset 1----------------------------------#
#-----------------------------------------------------------------------------------#
# Google Maps/Stratmen
get_map("Louisiana",
        zoom = 6,
        maptype = "toner-background") %>%
  
  # Ploting with ggmap
  ggmap(extent = "device")  +
  
geom_hurricane(data= ike,
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

#---------------------------Changing the scale to 0.5-------------------------------#

# Google Maps/Stratmen
get_map("Louisiana",
        zoom = 6,
        maptype = "toner-background") %>%
  
  # Ploting with ggmap
  ggmap(extent = "device")  +
  
  geom_hurricane(data= ike,
                 aes(x = longitude,
                     y = latitude,
                     r_ne = ne,
                     r_se = se,
                     r_nw = nw,
                     r_sw = sw,
                     fill = wind_speed,
                     color = wind_speed,
                     scale_radii = 0.5)) +
  
  scale_color_manual(name = "Wind speed (kts)",
                     values = c("red",
                                "orange",
                                "yellow")) +
  
  scale_fill_manual(name = "Wind speed (kts)",
                    values = c("red",
                               "orange",
                               "yellow"))
#-----------------------------------------------------------------------------------#
#-------------------------------Plot for Dataset 2----------------------------------#
#-----------------------------------------------------------------------------------#

get_map(c(-93.2, 27.5), # using exact coordinates in (lon, lat) format
        zoom = 6,
        maptype = "toner-background") %>%
  
  # Ploting with ggmap
  ggmap(extent = "device")  +
  
  geom_hurricane(data= ike2,
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

#---------------------------Changing the scale to 0.5--------------------------------#

# Google Maps/Stratmen
get_map(c(-93.2, 27.5), # using exact coordinates in (lon, lat) format
        zoom = 6,
        maptype = "toner-background") %>%
  
  # Ploting with ggmap
  ggmap(extent = "device")  +
  
  geom_hurricane(data= ike2,
                 aes(x = longitude,
                     y = latitude,
                     r_ne = ne,
                     r_se = se,
                     r_nw = nw,
                     r_sw = sw,
                     fill = wind_speed,
                     color = wind_speed,
                     scale_radii = 0.75)) +
  
  scale_color_manual(name = "Wind speed (kts)",
                     values = c("red",
                                "orange",
                                "yellow")) +
  
  scale_fill_manual(name = "Wind speed (kts)",
                    values = c("red",
                               "orange",
                               "yellow"))

#------------------------------------End of Script-------------------------------------#
