# Course: Mastering Software Development in R - Building Data Visualization Tools
# Peer Graded Assignment - Build a New Geom
# Submitted for Peer Review - 29th September 2021
# Storm name = IKE
directory <- "C://Ext_tracks//"
name <- 'IKE'
storm_year <- '2008'
storm_month <- '09'
storm_day <- '07'
storm_hour <- '06'
scale_radii <- 1.0    # Parameter which scales back the the radii to a percentage of the maximun radii

#  Storrm observaton creates a data frame containing 3 records for each different wind speed
storm_observation <- storm_data(directory, name, storm_year, storm_month, storm_day, storm_hour, scale_radii)

# Generate the apropriate map - with the correct centre orientation
map_data <- ggmap::get_map(location = c(lon = unique(storm_observation$longitude), lat = unique(storm_observation$latitude)), zoom = 6, maptype = "toner-2011", messaging = FALSE)
base_map <- ggmap::ggmap(map_data, extent = "device")

# Final R code to generate the wind radii chart
base_map +
  geom_hurricane(data = storm_observation, aes(x = longitude, y = latitude, r_ne = ne, r_se = se, r_nw = nw, r_sw = sw, fill = wind_speed, colour = wind_speed)) +
  scale_color_manual(name = "Wind speed (kts)", values = c("red", "orange", "yellow")) +
  scale_fill_manual(name = "Wind speed (kts)", values = c("red", "orange", "yellow"))


#' @title
#' Storm Observation
#'
#' @description
#' Storm Observation imports the underlying storm data and converts it to a single storm observation to be processed by the StatHurricane and GeomHurricane objects.
#'
#' @details
#' Storm Observation imports the underlying selected storm data and converts it to a dataframe comprising the following columns;
#' * stormid - Combination of storm name and date of the storm
#' * storm_name - Name of storm
#' * Date - Date the storm occurred on
#' * latitude - Latitude of the storm epicentre
#' * longitude - Longitude of the storm epicentre
#' * wind_speed - Measured wind speed
#' * ne - represents the maximum distance from the center of the storm which experienced the given wind speed in the north-east quadrant
#' * nw - represents the maximum distance from the center of the storm which experienced the given wind speed in the north-west quadrant
#' * se - represents the maximum distance from the center of the storm which experienced the given wind speed in the south-east quadrant
#' * sw - represents the maximum distance from the center of the storm which experienced the given wind speed in the south-west quadrant
#'
#' @param directory Path to the underlying Extended Best Tract dataset which covers Atlantic basin tropical storms since 1988 (can be downloaded from here: \url{http://rammb.cira.colostate.edu/research/tropical_cyclones/tc_extended_best_track_dataset/})
#' @param name Storm Name
#' @param storm_year Year of storm
#' @param storm_month Month of storm
#' @param storm_day Day of storm
#' @param storm_hour Hour storm occured
#' @param scale_radii scales the wind radii chart to a percent of the maximum radii. e.g. 0.6 will plot a wind radii chart
#' which maps 60% of the extent of the maximum wind radii in each quadrant.
#'
#' @importFrom readr read_fwf
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom magrittr %>%
#' @importFrom geosphere destPoint
#'
#' @return a dataframe with a record for each observed wind speed
#'
#' @examples
# directory_path <- 'C://HurricaneData//'
# name <- 'KATRINA'
# storm_year <- '2005'
# storm_month <- '08'
# storm_day <- '29'
# storm_hour <- '12'
# scale_radii <- 1.0
#' storm_observation <- storm_data('C://Ext_tracks//Data//', 'KATRINA', '2005', '08', '29', '12', '1.0')
#'
#' @name StormData
#'
#' @export
storm_data <- function(directory, name, storm_year, storm_month, storm_day, storm_hour, scale_radii){
  setwd(directory)
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
  ext_tracks <- readr::read_fwf("Week 4/ebtrk_atlc_1851_2019_old.txt",
                                readr::fwf_widths(ext_tracks_widths, ext_tracks_colnames),
                                na = "-99")
  storm_data <- ext_tracks %>% dplyr::filter(storm_name == name & year == storm_year & day == storm_day & month == storm_month & hour  == storm_hour)
  
  # Remove NA values
  storm_data[is.na(storm_data)] <- 0
  
  # Tidy the data into long format from the current wide format. Take values associated with columns
  storm_34 <- storm_data %>%
    dplyr::mutate(wind_speed = '34',
                  # stormidi2 = c(paste(storm_2008[1,'storm_id'], storm_2008[1,'storm_name'], sep='_'))) %>%
                  stormid = c(paste(storm_name, year, sep='-')),
                  Date = c(paste(c(paste(year, month, day, sep='-')), paste(hour, '00', '00', sep=':'))),
                  ne = radius_34_ne * scale_radii, nw = radius_34_nw * scale_radii, se=radius_34_se * scale_radii, sw=radius_34_sw * scale_radii) %>%
    dplyr::select ("stormid", "storm_name", "Date", "latitude", "longitude", "wind_speed", "ne", "nw", "se", "sw")
  storm_50 <- storm_data %>%
    dplyr::mutate(wind_speed = '50',
                  stormid = c(paste(storm_name, year, sep='-')),
                  Date = c(paste(c(paste(year, month, day, sep='-')), paste(hour, '00', '00', sep=':'))),
                  ne = radius_50_ne * scale_radii, nw = radius_50_nw * scale_radii, se=radius_50_se * scale_radii, sw=radius_50_sw * scale_radii) %>%
    dplyr::select ("stormid", "storm_name", "Date","latitude", "longitude", "wind_speed", "ne", "nw", "se", "sw")
  storm_64 <- storm_data %>%
    dplyr::mutate(wind_speed = '64',
                  # stormidi2 = c(paste(storm_2008[1,'storm_id'], storm_2008[1,'storm_name'], sep='_'))) %>%
                  stormid = c(paste(storm_name, year, sep='-')),
                  Date = c(paste(c(paste(year, month, day, sep='-')), paste(hour, '00', '00', sep=':'))),
                  ne = radius_64_ne * scale_radii, nw = radius_64_nw * scale_radii, se=radius_64_se * scale_radii, sw=radius_64_sw * scale_radii) %>%
    dplyr::select ("stormid", "storm_name", "Date", "latitude", "longitude", "wind_speed", "ne", "nw", "se", "sw")
  storm_observation <- rbind(storm_34, storm_50, storm_64)
  # Convert the wind speed observations to numeric values
  storm_observation$wind_speed <- as.numeric(storm_observation$wind_speed)
  # Latitudes have positive and negative values. Northern Hemisphere latitudes are positive, and negative latitudes occur in the Southern Hemisphere.
  # Longitudes have positive and negative values. Positive longitudes are in the Eastern Hemisphere (east of the Prime Meridian), and negative occur in the Western Hemisphere (west of 0º).
  storm_observation <- transform(storm_observation, longitude = -1*longitude)
}


#' @title
#' Wind_Radii Stat
#'
#' @description
#' StatHurricane creates the required data frame to be used by the GeomHurricane Geom.
#'
#' @details
#' StatHurricane converts the storm observation data into a dataframe which is then passed to the geom to be rendered graphically as a wind radii chart. The data passed to the stat consists of 3 records, 1 for each of the observed three wind speeds for wind radii (34 knots, 50 knots, and 64 knots). The stat then calculates the longitude and latitude values of the maximum distance observed for a given windspeed in each of the northeast, northwest, southeast, and southwest quadrants of the storm.
#' The layer function, Stat_hurricane builds the required layer for the GeomHurricane geom
#'
#' @param r_ne Maximum distance observed in the north east quadrant for a given wind speed
#' @param r_se Maximum distance observed in the south east quadrant for a given wind speed
#' @param r_nw Maximum distance observed in the north west quadrant for a given wind speed
#' @param r_sw Maximum distance observed in the south west quadrant for a given wind speed
#' @param fill vector of observed wind speeds (a required aesthetic for the Heom_Hurricane geom)
#' @param colour vector of observed wind speeds (a required aesthetic for the Heom_Hurricane geom)
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom magrittr %>%
#' @importFrom geosphere destPoint
#'
#' @name StatHurricane
#'
StatHurricane <- ggplot2::ggproto("_class" = "StatHurricane", "_inherit" = ggplot2::Stat,
                                  compute_group = function(data, scales, r_ne = ne, r_se = se, r_nw = nw, r_sw = sw, fill = wind_speed, colour = wind_speed) {
                                    # Determine epicentre of storm
                                    centre <- c(unique(data$x), unique(data$y))
                                    # Build table for each quadrant (ne, se, sw, nw)
                                    hurricane_data <- data.frame(lon=NULL, lat=NULL, id=NULL, wind_speed=NULL)
                                    vect_wind <- c(34,50,64)
                                    vect_quad <- c('r_ne','r_se','r_sw','r_nw')
                                    # Loop over each quadrant
                                    idx <- 1
                                    for (q in 1:4){
                                      # Loop through each wind speed reading
                                      for(ws in seq_along(vect_wind)){
                                        # Calculate distance from epicentre of storm
                                        quad <- vect_quad[q]
                                        distance <- data %>% dplyr::filter(fill == vect_wind[ws]) %>% dplyr::select (quad)
                                        if (q == 1) {
                                          deg=c(1:90)           # ne quadrant
                                        } else {
                                          if (q == 2) {
                                            deg=c(91:180)       # se quadrant
                                          } else {
                                            if (q == 3){
                                              deg=c(181:270)    # sw quadrant
                                            } else
                                              deg=c(271:360)    # nw quadrant
                                          }
                                        }
                                        target <- data %>% dplyr::filter(fill == vect_wind[ws]) %>%
                                          cbind(geosphere::destPoint(centre, b=deg, d=distance.km(distance)), id=rep(idx, 90)) %>%
                                          dplyr::select (lon, lat, id, wind_speed = fill)
                                        hurricane_data <- rbind(hurricane_data, target)
                                        # Add epicentre point for each quadrant/wind speed combination
                                        hurricane_data <- rbind(hurricane_data, c(lon=centre[1], lat=centre[2], id=idx, wind_speed=vect_wind[ws]))
                                        idx <- idx + 1
                                      }
                                    }
                                    hurricane_data <- cbind(hurricane_data, colour = hurricane_data$wind_speed)
                                    hurricane_data <- setNames(hurricane_data, c("x", "y", "group", "fill", "colour"))
                                    hurricane_data$fill <- as.factor(hurricane_data$fill)
                                    hurricane_data$colour <- as.factor(hurricane_data$colour)
                                    data <- hurricane_data
                                  },
                                  required_aes = c("x", "y", "r_ne", "r_se", "r_nw", "r_sw", "fill", "colour")
)

stat_Hurricane <- function(geom = "polygon", data = NULL, mapping = NULL,
                           position = "identity", na.rm = F, inherit.aes = TRUE,
                           show.legend = NA, ...){
  
  layer(geom = GeomHurricane, stat = StatHurricane,
        data = data, mapping = mapping, position = position,
        inherit.aes = inherit.aes, show.legend = show.legend,
        params = list(na.rm = na.rm, ...))
}


#' @title
#' Wind_Radii Geom
#'
#' @description
#' GeomHurricane draws the wind radii chart
#'
#' @details
#' GeomHurricane draws a converts the storm observation data into a dataframe which is then passed to the geom to be rendered graphically as a wind radii chart. A dataframe consisting of the latidude, longitude, wind speed and colour for each geographical point in a quadrant is passed to the geom from the StatHurricane stat. The geom draws different coloured polygons in each quadrant representing the furthest observed distance of a given wind speed.
#'
#' @param x aesthetic containing the longitudinal coordinate passed to the geom from StatHurricane for a given wind speed/ distance /quadrant
#' @param y aesthetic containing the latitudinal coordinate passed to the geom from StatHurricane for a given wind speed/ distance /quadrant
#' @param fill Wind speed aesthetic used to derive each longitudinal/latitudinal coordinate in a given quadrant
#' @param colour colour aesthetic used when drawing the map ploygons. This is overriden with the scale_color_manual and scale_fill_manual functions
#'
#' @name GeomHurricane
#'
#' @export
GeomHurricane <- ggplot2::ggproto("GeomHurricane", ggplot2::GeomPolygon,
                                  required_aes = c("x", "y", "fill", "colour"),
                                  default_aes = ggplot2::aes(size = 0.5, linetype = 1, alpha = 0.75)
)

geom_hurricane <- function(mapping = NULL, data = NULL,
                           position = "identity", na.rm = FALSE, show.legend = NA,
                           inherit.aes = TRUE, ...) {
  layer(
    stat = StatHurricane, geom = GeomHurricane, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

#' @title
#' Distance Calculation
#'
#' @description
#' Converts nautical miles to kilometres
#'
#' @details
#' The original Extended Best Tract dataset \url{http://rammb.cira.colostate.edu/research/tropical_cyclones/tc_extended_best_track_dataset/} reports observed distances in nautical miles. These need to be converted to kilometres as this package uses the destPoint function from the geosphere package which uses kilometres as the base measure.
#' 1 nautical miles = 1852 metres
#'
#' @param x A number representing the distance (in nautical miles) to be converted to kilometres
#'
#' @note The function is an implementation within the Wind_Radii package. It is not explicitly exported.
#'
#' @return The distance entered as a parameter converted to kilometres
#'
#' @examples
#' distance.km(100)
#'
#' @name Distance
#'
distance.km <- function(x){
  # 1 nautical miles = 1852 metres
  return ( x * 1852)
}
