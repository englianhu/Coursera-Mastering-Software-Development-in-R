library(readr)
library(dplyr)
library(grid)
library(ggplot2)
library(geosphere)
library(roxygen2)

# read in ext_tracks file
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

# Create functions to tidy data for use in the hurricane geom
#' Storm
#'
#' @description Creates a data from with name, windspeed, and distances of the windspeeds in certain directions
#' 
#' @param storm character string of storm name in all capitals.
#' @param year character string of the year you would like data on the specified storm.
#' @param month character string of the month you would like data on the specified storm.
#' @param day character string of the day you would like data on the specified storm.
#' @param hour character string hour of the day you would like data on the specified storm.
#' 
#' @examples \dontrun{Ike <- storm("IKE", "2008", "09", "13", "06")}
#' @examples \dontrun{Katrina <- storm("KATRINA", "2005", "08", "29", "12")}
#' 
#' @importFrom dplyr mutate
#' @importFrom dplyr group_by
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr summarize
#' 
#' @return A data frame with storm name, year, date, latitude, longitude, wind_speeds in ne, nw, se, sw directions.
#' 
#' @export
storm <- function(storm, year, month, day, hour){
  id <- paste0(storm, "-", year, sep = "")
  date_time <- paste0(year, "-", month, "-", day, " ", hour, ":00:00", sep = "")
  # create desired table
  storm_info <- ext_tracks %>%
    dplyr::mutate(storm_id = paste(storm_name, "-", year, sep = ""),
                  date = paste0(year, "-", month, "-", day, " ", hour, ":00:00", sep = ""),
                  longitude = as.numeric(longitude*(-1))) %>%
    dplyr::group_by(storm_id) %>%
    dplyr::filter(storm_id == id, date == date_time) %>%
    dplyr::select(storm_id, date, latitude, longitude, radius_34_ne, radius_34_se, radius_34_sw, radius_34_nw,
                  radius_50_ne, radius_50_se, radius_50_sw, radius_50_nw, radius_64_ne, radius_64_se, radius_64_sw, radius_64_nw) %>%
    dplyr::summarize(storm_id, date, latitude, longitude, wind_speed = c(30, 50, 64),
                     ne = c(radius_34_ne, radius_50_ne, radius_64_ne), nw = c(radius_34_nw, radius_50_nw, radius_64_nw),
                     se = c(radius_34_se, radius_50_se, radius_64_se), sw = c(radius_34_sw, radius_50_sw, radius_64_sw))
  return(storm_info)
}


#' hurricane_perimeter
#'
#' @description finds the points of the perimeter of a polygon from given longitude, latitude, direction, and nautical miles.
#'
#' @param storm A dataframe of the specified storm. Must have numeric columns for longitude, latitude, ne, se, sw, nw.
#' @param i The row number of your dataframe you would like to calculate a parameter for.
#' @param scale_radius A number from 0-1. Scales the radius accordingly with 1 being the full radius.
#'
#' @examples \dontrun{hurricane_perimeter(Katrina, 1)}
#' @examples \dontrun{hurricane_perimeter(Ike, 2)}
#'
#' @importFrom geosphere destPoint
#'
#' @return a 2 column data.frame of all the longitudes and latitudes of each calculated perimeter point.
#'
#' @export
hurricane_perimeter <- function(storm, i, scale_radius = 1){
  # find points along the perimeter and convert to meters from nautical miles, while also scaling perimeter to desired length.
  center <- c(storm$longitude[i], storm$latitude[i])
  if(scale_radius > 1 | scale_radius < 0){
    stop("Only allowed to scale between 1 and 0")
  }
  perimeter_ne <- geosphere::destPoint(p = center, b = 0:90, d = storm$ne[i]*1852*scale_radius)
  perimeter_se <- geosphere::destPoint(p = center, b = 90:180, d = storm$se[i]*1852*scale_radius)
  perimeter_sw <- geosphere::destPoint(p = center, b = 180:270, d = storm$sw[i]*1852*scale_radius)
  perimeter_nw <- geosphere::destPoint(p = center, b = 270:360, d = storm$nw[i]*1852*scale_radius)
  perimeter <- as.data.frame(rbind(perimeter_ne, perimeter_se, perimeter_sw, perimeter_nw))
  return(perimeter)
}


# Create the actual hurricane geom
#' GeomHurricane
#' 
#' @description A geom that maps the distances of windspeeds from the center of a hurricane.
#' 
#' @note Parameters that can be used with the geom are documented under the geom_Hurricane function.
#' 
#' @importFrom ggplot2 ggproto
#' @importFrom ggplot2 GeomPolygon
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 draw_key_polygon
#' @importFrom ggplot2 coord_munch
#' 
#' @return A polygon grob that maps windspeed data to a graph
GeomHurricane <- ggplot2::ggproto("GeomHurricane", ggplot2::GeomPolygon,
                         required_aes = c("x", "y"),
                         default_aes = ggplot2::aes(color = "red", fill = "red"),
                         draw_key = ggplot2::draw_key_polygon,
                         draw_panel = function(data, panel_scales, coord){
  coords <- ggplot2::coord_munch(coord, data, panel_scales)
  coords$alpha <- 0.5
  polygonGrob(
    x = coords$x,
    y = coords$y,
    gp = grid::gpar(alpha = coords$alpha, col = coords$col, fill = coords$fill)
  )
}
)

# Create the function for use of the hurricane geom
#' geom_Hurricane
#'
#' @description Adds a hurricane geom to a ggplot object.
#'
#' @param mapping aesthetics to be mapped to GeomHurricane by use of aes() function.
#' @param data A data frame from which the data is mapped.
#' @param stat transforms the given data frame by given statistic
#' @param position Changes the position
#' @param show.legend Asks to show a legend
#' @param na.rm Removes all NA's from the data if TRUE
#' @param inherit.aes Inherits aesthetics from parent geom if TRUE
#'
#' @examples
#' \dontrun{geom_Hurricane(data = hurricane_perimeter(Katrina, 1), mapping = aes(x = lon, y = lat), color = "red", fill = "red")}
#' \dontrun{geom_Hurricane(data = hurricane_perimeter(Ike, 2), mapping = aes(x = lon, y = lat), color = "red", fill = "red")}
#'
#' @importFrom ggplot2 layer
#'
#'
#' @return Returns a geomHurricane polygon mapped with longitude and latitude.
#'
#' @export
geom_Hurricane <- function(mapping = NULL, data = NULL, stat = "identity",
                           position = "identity", show.legend = NA,
                           na.rm = FALSE, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomHurricane,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

# use the functions and geom
Ike <- storm("IKE", "2008", "09", "13", "06")

pdf("HurricaneIke.pdf")
# add in map
ggplot2::map_data("state") %>%
dplyr::filter(region %in% c("texas", "mississippi", "oklahoma", "louisiana", "arkansas")) %>%
ggplot2::ggplot(ggplot2::aes(x = long, y = lat)) +
  ggplot2::geom_polygon(ggplot2::aes(group = group, fill = group)) +
  geom_Hurricane(data = hurricane_perimeter(Ike, 1), mapping = ggplot2::aes(x = lon, y = lat), color = "red", fill = "red") +
  geom_Hurricane(data = hurricane_perimeter(Ike, 2), mapping = ggplot2::aes(x = lon, y = lat), color = "orange", fill = "orange") +
  geom_Hurricane(data = hurricane_perimeter(Ike, 3), mapping = ggplot2::aes(x = lon, y = lat), color = "yellow", fill = "yellow") +
  ggplot2::ggtitle("Hurricane Ike: August 13, 2008; 6:00")

# map with scale of 0.5
ggplot2::map_data("state") %>%
  dplyr::filter(region %in% c("texas", "mississippi", "oklahoma", "louisiana", "arkansas")) %>%
  ggplot2::ggplot(ggplot2::aes(x = long, y = lat)) +
  ggplot2::geom_polygon(ggplot2::aes(group = group, fill = group)) +
  geom_Hurricane(data = hurricane_perimeter(Ike, 1, scale_radius = 0.5), 
                 mapping = ggplot2::aes(x = lon, y = lat), color = "red", fill = "red") +
  geom_Hurricane(data = hurricane_perimeter(Ike, 2, scale_radius = 0.5), 
                 mapping = ggplot2::aes(x = lon, y = lat), color = "orange", fill = "orange") +
  geom_Hurricane(data = hurricane_perimeter(Ike, 3, scale_radius = 0.5), 
                 mapping = ggplot2::aes(x = lon, y = lat), color = "yellow", fill = "yellow") +
  ggplot2::ggtitle("Hurricane Ike: August 13, 2008; 6:00; scale_radius = 0.5")
dev.off()



