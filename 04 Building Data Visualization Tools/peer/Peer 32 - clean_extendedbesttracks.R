library(tidyverse)
library(lubridate)
library(stringr)
library(ggplot2)
library(geosphere)
#' Reads the storm data from file
#' 
#' @importFrom readr read_fwf
#' 
#' @param path path to ext tracks file
#' 
#' @examples
#' /dontrun{
#'   ext_tracks <- read_storm_data("data/ebtrk_atlc_1988_2015.txt")
#' }
read_storm_data <- function(path) {
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
  
  ext_tracks <- readr::read_fwf(path, 
                                fwf_widths(ext_tracks_widths, ext_tracks_colnames),
                                na = "-99") 
}

#' Tidy the storm data for plotting
#' 
#' @importFrom tidyr spread gather separate
#' @importFrom dplyr mutate select group_by summarise ungroup
#' @importFrom lubridate make_datetime 
#' 
#' @param ext_tracks_data ext tracks data created by read_storm_data
#' 
#' @examples
#' /dontrun{
#'   ext_tracks <- read_storm_data("data/ebtrk_atlc_1988_2015.txt")
#'   storm_data <- tidy_storm_data(ext_tracks)
#' }
tidy_storm_data <- function(ext_tracks_data) {
  ext_tracks_data %>% 
    dplyr::mutate(date=lubridate::make_datetime(as.integer(year), as.integer(month), as.integer(day), as.integer(hour)),
                  storm_id=paste0(storm_name, "_", year),
                  longitude=if_else(longitude < 180, longitude * -1, longitude)) %>% 
    dplyr::select(storm_id,date, latitude, longitude, starts_with("radius")) %>% 
    tidyr::gather(key="radius_wind_speed", value="value", -storm_id, -date, -latitude, -longitude) %>% 
    tidyr::separate(radius_wind_speed, c(NA, "wind_speed", "direction"), sep = "_") %>% 
    dplyr::select(storm_id, date, longitude, latitude, wind_speed, direction, value) %>% 
    dplyr::group_by(storm_id, date, latitude, longitude, wind_speed, direction) %>% 
    dplyr::summarise(value=sum(value, na.rm=TRUE)) %>% 
    dplyr::ungroup() %>%
    dplyr::mutate(wind_speed=as.factor(wind_speed)) %>% 
    tidyr::spread(key=direction, value=sum(value))
}

get_hurricane <- function(storm_data, name, year, time) {
  storm_data %>% 
    dplyr::filter(storm_id %in% paste0(name, "_", year), 
                  date %in% lubridate::ymd_hm(time))
}

#' Creates GeomHurricane proto
#' 
#' @importFrom ggplot2 ggproto Geom
#' @importFrom dplyr rename
#' @importFrom grid polygonGrob gpar 
#' @importFrom tibble as_tibble 
#' @importFrom geosphere destPoint
#' 
#' @examples
#' /dontrun{
#'   geom_hurricane(data = data_ike,
#'                  aes(x = longitude, y = latitude, 
#'                  r_ne = ne, r_se = se, r_nw = nw, r_sw = sw,
#'                  fill = wind_speed, color = wind_speed, alpha = 0.5, 
#'                  scale_radii = 1)) 
#' }
GeomHurricane <- 
  ggproto("GeomHurricane", 
          ggplot2::Geom,
          required_aes = c("x", "y", "r_ne", "r_se", "r_sw", "r_nw"),
          default_aes = aes(color = "grey", fill = 1, size = 1, 
                            alpha = 0.75, scale_radii = 1),
          draw_key = draw_key_polygon,
          draw_group = function(data, panel_scales, coord){
            
            create_wind_radii <- function(centre, wind_speed_radius, degrees) {
              
              NAUTICAL_MILES_TO_METRES <- 1852
              
              geosphere::destPoint(centre,
                        b=degrees,
                        d=wind_speed_radius * NAUTICAL_MILES_TO_METRES) %>%
                rbind(centre) %>%
                tibble::as_tibble() %>% 
                dplyr::rename(x="lon", y="lat")
            }
            
            centre <- c(data[1,]$x, data[1,]$y)
            
            data_ne <- create_wind_radii(centre, data[1, ]$r_ne * data[1,]$scale_radii,  1:90)
            data_se <- create_wind_radii(centre, data[1, ]$r_se * data[1,]$scale_radii, 90:180)
            data_sw <- create_wind_radii(centre, data[1, ]$r_sw * data[1,]$scale_radii,180:270)
            data_nw <- create_wind_radii(centre, data[1, ]$r_nw * data[1,]$scale_radii,270:360)
            
            data_combined <- rbind(data_ne,data_se,data_sw,data_nw)
            coords <- coord$transform(data_combined, panel_scales)
            
            grid::polygonGrob(x = coords$x,
                              y = coords$y,
                              gp = grid::gpar(col = data[1,]$color, 
                                              fill = data[1,]$fill, 
                                              alpha = data[1,]$alpha))
          }
  )

#' Creates lggplot2 layer for the GeomHurricane prot
#' 
#' @importFrom ggplot2 layer
#' 
#' @param mapping Aesthetic mappings.
#' @param data The hurricane data to include in the layer.
#' @param stat The statistical transformation to use on the data for this layer, as a string.
#' @param position Adjust position.
#' @param na.rm If FALSE, missing values removed with warning. If TRUE, missing valuessilently removed.
#' @param show.legend TRUE includes legend, FALSE/NA doesn't
#' @param inherit.aes FALSE overrides the default aesthetics
#' 
#' @export
geom_hurricane <- function(mapping = NULL, data = NULL, stat = "identity",
                           position = "identity", na.rm = FALSE,
                           show.legend = NA, inherit.aes = TRUE, ...){
  ggplot2::layer(
    geom = GeomHurricane, 
    mapping = mapping,
    data = data, 
    stat = stat, 
    position = position,
    show.legend = show.legend, 
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm,...)
  )
}

### Printing Hurricane IKE ##############################################################
ext_tracks <- read_storm_data("data/ebtrk_atlc_1988_2015.txt")
storm_data <- tidy_storm_data(ext_tracks)
data_katrina <- get_hurricane(storm_data, "KATRINA", "2005", "2005-08-29-12-00")
data_ike <- get_hurricane(storm_data, "IKE", "2008", "2008-09-13-12-00")

png(filename="IKE-2008.png")
get_map("Louisiana", zoom = 6, maptype = "toner-background") %>%
  ggmap() + 
  geom_hurricane(data = data_ike,
                 aes(x = longitude, y = latitude, 
                     r_ne = ne, r_se = se, r_nw = nw, r_sw = sw,
                     fill = wind_speed, color = wind_speed, alpha = 0.5, scale_radii = 1)) +
  scale_color_manual(name = "Wind Speed (kts)", values = c("red", "orange", "yellow")) + 
  scale_fill_manual(name = "Wind Speed (kts)", values = c("red", "orange", "yellow")) +
  scale_alpha(guide = 'none') +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Hurricane Ike - 2008")

dev.off()
