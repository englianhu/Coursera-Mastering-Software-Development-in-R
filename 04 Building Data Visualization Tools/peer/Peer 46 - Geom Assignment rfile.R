# Code to prepare `DATASET` dataset ----

# no function was created in order to prepare the data
# this section is to be saved in data-raw folder

usethis::use_pipe(export = TRUE)

setwd("~/R/hurricane/data-raw")

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

ext_tracks <- readr::read_fwf("_7ed6a595f3e1ac944ccbb1f07db4caae_hurricanes_data.zip",
                              readr::fwf_widths(ext_tracks_widths, ext_tracks_colnames), na = "-99")

storm_data <- ext_tracks %>%
  dplyr::mutate(
    storm_id = paste(
      stringr::str_to_title(storm_name),year,sep = "-"), #the same storm name can be used in different years, so this will allow for the unique identification of a storm
    date = as.POSIXct(paste0(year, "-", month, "-", day," ", hour,":00:00")),
    longitude = -longitude) %>% # negative values for locations in the Western hemisphere
  dplyr::select(storm_id, date, latitude, longitude, starts_with("radius")) %>%
  tidyr::pivot_longer(starts_with("radius"),names_to = "quadrant",values_to = "radius") %>%
  tidyr::separate(quadrant,into = c(NA,"wind_speed","quadrant") ,sep = "_") %>%
  tidyr::pivot_wider(names_from = "quadrant",values_from = "radius")

readr::write_csv(storm_data,"storm_data.csv")

usethis::use_data(storm_data, overwrite = TRUE)

# Function to subset storm_data ----

#' storm_observation
#'
#' Taking the dataset `storm_data`, this function creates a subset of a specific hurricane and a single observation time for that hurricane.
#'
#'
#' @param hurricane_id a string, Hurricane-year. The first character uppercase.
#' @param date_time a string, in yyyy-mm-dd hh:mm:ss format
#'
#' @return a data frame
#'
#' @examples
#' \dontrun{
#' storm_observation("Ike-2008","2008-09-13 12:00:00")
#' storm_observation("Katrina-2005","2005-08-29 12:00:00")}
#'
#'
#' @export

storm_observation <- function(hurricane_id="Katrina-2005", date_time ="2005-08-29 12:00:00"){
  storm_data %>%
    dplyr::filter(storm_id == hurricane_id, date == date_time)
}

# storm_data documentation ----

#' Data on all storms in the Atlantic basin from 1988 to 2015.
#'
#' The dataset is based on the Extended Best Tract dataset and it was manipulated for the assignment "Build a New Geom" - part of Johns Hopkins University & Cousera Course "Building Data Visualization Tools".
#'
#' @format A data frame with 35 472 rows and 9 variables
#' \describe{
#'   \item{storm_id}{name of the hurricane and year (e.g. Katrina-2005)}
#'   \item{date}{date and time of the record (e.g. 2008-09-13 12:00:00)}
#'   \item{latitude}{latitudeof the center of the hurricane}
#'   \item{longitude}{longitude of the center of the hurricane}
#'   \item{wind_speed}{intensity of the wind in knots (34, 50, 60)}
#'   \item{ne}{distance (in meters) from the center of the storm of any location that experienced x-knot winds in the Northeast quadrant}
#'   \item{se}{distance (in meters) from the center of the storm of any location that experienced x-knot winds in the Southeast quadrant}
#'   \item{sw}{distance (in meters) from the center of the storm of any location that experienced x-knot winds in the Southwest quadrant}
#'   \item{nw}{distance (in meters) from the center of the storm of any location that experienced x-knot winds in the Northwest quadrant}
#' }
#' @source \url{<http://rammb.cira.colostate.edu/research/tropical_cyclones/tc_extended_best_track_dataset/>}

"storm_data"

# New Stat & geom_hurricane setup ----

#' My approach was to create a new Stat, following the explanations in
#' \url{https://cran.r-project.org/web/packages/ggplot2/vignettes/extending-ggplot2.html}."

StatHurricane <- ggplot2::ggproto("StatHurricane", ggplot2::Stat,

                                  #' StatHurricane setup
                                  #'
                                  #' @import ggplot2
                                  #'
                                  #' @import geosphere
                                  #'
                                  #' @export
                                  #'
                                  #'
                                  compute_group = function(data, scales, scale_radii) {

                                    quadrant <- data[c("r_ne","r_se","r_sw","r_nw")]
                                    output<-list()
                                    for (i in 1:4){
                                      geosphere::destPoint(p = c(data$x,data$y),
                                                           b = ((i-1)*90):(90*i),
                                                           d = (quadrant[,i] * 1852 * scale_radii)) %>%
                                        tibble::as_tibble() %>%
                                        rbind(output)-> output
                                    }
                                    data <- data.frame(output)
                                    names(data) <- c("x", "y")
                                    data
                                  },
                                  required_aes = c("x", "y","r_ne","r_se","r_sw","r_nw")
)

#' geom_hurricane
#'
#' This function returns a wind radii chart.
#' You should provide a dataset for a specific hurricane and a single observation time for that hurricane using.
#'
#' @param mapping default
#' @param data default
#' @param geom default
#' @param position default
#' @param show.legend default
#' @param inherit.aes default
#' @param alpha 0.5 transparency
#' @param scale_radii allows the user to plot wind radii charts with the radii scaled back to a certain percent of the maximum radii
#' @param ... default
#'
#'
#'
#' @return a wind radii chart
#' @export
#'
#' @examples
#' \dontrun{
#' get_map("Louisiana", zoom = 6, maptype = "toner-background") %>%
#' ggmap(extent = "device") +
#' geom_hurricane(data = storm_observation,
#' aes(x = longitude, y = latitude,
#' r_ne = ne, r_se = se, r_nw = nw, r_sw = sw,
#' fill = wind_speed, color = wind_speed))
#' }

geom_hurricane <- function(mapping = NULL,
                           data = NULL,
                           geom = "polygon",
                           position = "identity",
                           show.legend = NA,
                           inherit.aes = TRUE,
                           scale_radii = 1,
                           alpha = 0.7,
                           ...)
{

  ggplot2::layer(
    stat = StatHurricane,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(alpha=alpha,scale_radii = scale_radii, ...)
  )
}

# Ike Hurricane Wind Radii ----
library(ggmap)

register_google(key = "mycode")


ike <- storm_observation("Ike-2008","2008-09-13 12:00:00")

get_map("Louisiana", zoom=6,maptype = "toner-background", source = "stamen") %>%
  ggmap()+
  geom_hurricane(data = ike,
                 ggplot2::aes(x = longitude, y = latitude,
                              r_ne = ne, r_se = se, r_nw = nw, r_sw = sw,
                              fill = wind_speed, color = wind_speed),
                 scale_radii = 1)+
  ggplot2::scale_color_manual(name = "Wind speed (kts)",
                              values = c("red", "orange", "yellow")) +
  ggplot2::scale_fill_manual(name = "Wind speed (kts)",
                             values = c("red", "orange", "yellow"))

get_map("Louisiana", zoom=6,maptype = "toner-background", source = "stamen") %>%
  ggmap()+
  geom_hurricane(data = ike,
                 ggplot2::aes(x = longitude, y = latitude,
                              r_ne = ne, r_se = se, r_nw = nw, r_sw = sw,
                              fill = wind_speed, color = wind_speed),
                 scale_radii = 0.5)+
  ggplot2::scale_color_manual(name = "Wind speed (kts)",
                              values = c("red", "orange", "yellow")) +
  ggplot2::scale_fill_manual(name = "Wind speed (kts)",
                             values = c("red", "orange", "yellow"))
