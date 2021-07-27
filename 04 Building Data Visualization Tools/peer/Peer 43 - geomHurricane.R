# Init -------------------------------------------------------------------------

library(magrittr)
library(ggplot2)

# Create Data ------------------------------------------------------------------

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

ext_tracks <- readr::read_fwf("data/ebtrk_atlc_1988_2015.txt", 
                              readr::fwf_widths(ext_tracks_widths, ext_tracks_colnames),
                              na = "-99")

#' Pull Observation 
#'
#' A function that allows the user to pull individual or multiple observations  
#' from United States National Hurricane Center Extended Best Track Dataset files
#' in a format that allows wind radii charts to be plotted using \code{geom_hurricane}.
#'
#' @param data A data frame containing data imported from the Extended Best Track
#'    Dataset text file.
#' @param storm A character string identifying the name of the storm.
#' @param year The year of the storm, useful when the same name is used more than once.
#' @param dt A character string of the date-time of the observation to pull
#' 
#' @return This function returns a data frame of storm observations that match the
#' paramters provided.
#'
#' @examples
#' pull_obs(data = data)
#' pull_obs(data = data, storm = "Katrina")
#' pull_obs(data = data, storm = "Katrina", year = 2005)
#' pull_obs(data = data, storm = "Katrina", dt = "2005-08-25 12:00:00")
#'
#' @importFrom magrittr "%>%"
#' 
#' @export
pull_obs <- function(data, storm = "", year = "", dt = "") {
  data %>%
    dplyr::mutate(storm_id = paste(stringr::str_to_title(storm_name), year, sep = "-"),
                  longitude = -longitude,
                  date = as.POSIXct(paste(year, month, day, hour, sep = "-"),
                                    format = "%Y-%m-%d-%H")) %>%
    dplyr::select(storm_id, date, latitude, longitude, starts_with("radius")) %>%
    tidyr::gather(starts_with("radius"), key = "key", value = "value") %>%
    dplyr::mutate(key = stringr::str_split(key, "_", simplify = TRUE),
                  wind_speed = as.factor(key[,2]),
                  key = key[,3]) %>%
    tidyr::spread(key = key, value = value) %>%
    dplyr::filter(grepl(stringr::str_to_title(storm), storm_id) &
                    grepl(year, storm_id) &
                    grepl(dt, date))
}

storm_observation <- pull_obs(ext_tracks, storm = "ike", dt = "2008-09-13 00:00:00")
storm_observation

# Create Stat + Geom -----------------------------------------------------------

StatHurricane <- ggplot2::ggproto("StatHurricane", Stat, 
                  compute_group = function(data, scales, scale_radii = 1) {
                  factor <- 1852 * scale_radii
                  ne <- geosphere::destPoint(c(data$x, data$y), 0:90, data$r_ne * factor)
                  se <- geosphere::destPoint(c(data$x, data$y), 90:180, data$r_se * factor)
                  sw <- geosphere::destPoint(c(data$x, data$y), 180:270, data$r_sw * factor)
                  nw <- geosphere::destPoint(c(data$x, data$y), 270:360, data$r_nw * factor)
                  df <- data.frame(rbind(ne, se, sw, nw))
                  colnames(df) <- c("x", "y")
                  df
                  },
                  required_aes = c("x", "y", "r_ne", "r_se", "r_nw", "r_sw", "fill")
)

stat_hurricane <- function(mapping = NULL, data = NULL, geom = "hurricane",
                           position = "identity", show.legend = NA, 
                           inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatHurricane, 
    data = data, 
    mapping = mapping, 
    geom = geom, 
    position = position, 
    show.legend = show.legend, 
    inherit.aes = inherit.aes,
    params = list(scale_radii = scale_radii, ...)
  )        
}

GeomHurricane <- ggplot2::ggproto("GeomHurricane", GeomPolygon)

#' Hurricane Geom
#'
#' A ggplot2 geom that plots wind radii charts on maps showing the wind speeds recorded
#' for hurricanes in four compass directions at a single location and point in time.
#' Takes as input a data frame of a single storm observation extracted from United
#' States National Hurricane Center Extended Best Track data using the \code{pull_obs}
#' function.
#'
#' @param x,y Required, must be wrapped in aes(). Columns of the dataframe containing
#'    longitude and latitude values.
#' @param r_ne,r_se,r_sw,r_nw Required, must be wrapped in aes(). Columns of the dataframe 
#'    containing radii values for each of the four compass points.
#' @param fill,color Required, must be wrapped in aes(). Column of the dataframe containing
#'    the wind speed levels.
#' @param scale_radii Optional. A numeric value between 0 and 1 that scales the wind radii
#'    as a proportion of the maximum.
#'    
#' @return This function returns a ggplot2 wind radii chart object.
#'
#' @examples
#' ggplot() +
#'  geom_hurricane(data = storm_observation,
#'    aes(x = longitude, y = latitude, 
#'    r_ne = ne, r_se = se, r_nw = nw, r_sw = sw,
#'    fill = wind_speed, color = wind_speed))
#'    
#' ggplot() +
#' geom_hurricane(data = storm_observation,
#'    scale_radii = 0.8,
#'    aes(x = longitude, y = latitude, 
#'    r_ne = ne, r_se = se, r_nw = nw, r_sw = sw,
#'    fill = wind_speed, color = wind_speed)) + 
#'    scale_color_manual(name = "Wind speed (kts)",
#'    values = c("red", "orange", "yellow")) + 
#'    scale_fill_manual(name = "Wind speed (kts)", 
#'    values = c("red", "orange", "yellow"))
#'
#' @import ggplot2
#' 
#' @export
geom_hurricane <- function(mapping = NULL, data = NULL, 
                           position = "identity", show.legend = NA, 
                           na.rm = FALSE, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    data = data, 
    mapping = mapping,
    stat = StatHurricane,
    geom = GeomHurricane,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(alpha = 0.6, ...)
  )
}

# Output -----------------------------------------------------------------------

library(ggmap)

# Code below produces output of PDF
get_stamenmap(bbox = c(left = -99, bottom = 23, right = -82, top = 33),
              zoom = 6, maptype = "toner-background") %>%
  ggmap(extent = "device") +
  geom_hurricane(data = storm_observation,
                 aes(x = longitude, y = latitude, 
                     r_ne = ne, r_se = se, r_nw = nw, r_sw = sw,
                     fill = wind_speed, color = wind_speed)) + 
  scale_color_manual(name = "Wind speed (kts)", 
                     values = c("red", "orange", "yellow")) + 
  scale_fill_manual(name = "Wind speed (kts)", 
                    values = c("red", "orange", "yellow"))

# Same output scaled by scale_radii = 0.8
get_stamenmap(bbox = c(left = -99, bottom = 23, right = -82, top = 33),
              zoom = 6, maptype = "toner-background") %>%
  ggmap(extent = "device") +
  geom_hurricane(data = storm_observation,
                 scale_radii = 0.8,
                 aes(x = longitude, y = latitude, 
                     r_ne = ne, r_se = se, r_nw = nw, r_sw = sw,
                     fill = wind_speed, color = wind_speed)) + 
  scale_color_manual(name = "Wind speed (kts)", 
                     values = c("red", "orange", "yellow")) + 
  scale_fill_manual(name = "Wind speed (kts)", 
                    values = c("red", "orange", "yellow"))
