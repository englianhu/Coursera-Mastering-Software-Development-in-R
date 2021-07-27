#' Reading the Hurricane data 
#'
#' @param file Giving the file 
#'
#' @return The dataframe with appropriate column names 
#' @export
#'
#' @examples
read_hurricane <- function(file){
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
    
    data <- read_fwf(file, 
                     fwf_widths(ext_tracks_widths, ext_tracks_colnames),
                     na = "-99")
    
}

#' Tidy data of Hurricance
#'
#' @param data Giving the data frame of untidy data
#'
#' @return This give the clen tidy data
#' @export
#'
#' @examples

tidy_hurricane <- function(data){
    ## Creating date column
    data$date <- paste0(data$year,"-",data$month,"-",data$day," ",data$hour,":00:00")
    data$date <- ymd_hms(data$date)
    ## Creating storm id
    data$storm_id <- paste0(data$storm_name,"-",data$year)
    ## Converting longitude to negative as mention (It's in Northern Hemisphere)
    data$longitude <- as.numeric(-data$longitude)
    
    ## Data Selection for Wind Speed 34 Knots 
    data_ws_34 <- data %>%
        select(storm_id, date, latitude, longitude,starts_with("radius_34_")) %>%
        rename(ne = radius_34_ne, se = radius_34_se, nw = radius_34_nw, sw = radius_34_sw) %>%
        mutate(wind_speed = 34)
    
    ## Data Selection for Wind Speed 50 Knots 
    data_ws_50 <- data %>%
        select(storm_id, date, latitude, longitude,starts_with("radius_50_")) %>%
        rename(ne = radius_50_ne, se = radius_50_se, nw = radius_50_nw, sw = radius_50_sw) %>%
        mutate(wind_speed = 50)
    
    ## Data Selection for Wind Speed 64 Knots
    data_ws_64 <- data %>%
        select(storm_id, date, latitude, longitude,starts_with("radius_64_")) %>%
        rename(ne = radius_64_ne, se = radius_64_se, nw = radius_64_nw, sw = radius_64_sw) %>%
        mutate(wind_speed = 64)
    
    ## Now we will combine all the 3 speeds and arrange it with date time
    data_clean <- rbind(data_ws_34,data_ws_50,data_ws_64) %>% arrange(date)
    
}


#' Getting the Hurricane 
#'
#' @param data_clean Give the clean dataframe
#' @param storm_id Give the Hurricane or Storm id
#' @param time Give the observation time
#'
#' @return
#' @export
#'
#' @examples

Hurricane <- function(data_clean,storm_year,time){
    Hurricane_name <- data_clean %>% 
        filter(storm_id == storm_year & date ==ymd_hms(time))
    Hurricane_name$wind_speed <- as.factor(Hurricane_name$wind_speed)
    return(Hurricane_name)
}

###### Now Will Create Stat Hurricane to get the quadrant 

StatHurricane <- 
    ggplot2::ggproto("StatHurricane", Stat,
                     compute_group = function(data, scales, scale_radii) {
                         lon <- data$x[1]
                         lat <- data$y[1]
                         new_arc <- function(direction, radius){
                             my_arc <- destPoint(c(lon, lat),
                                                            case_when(direction == "r_ne" ~ 0:90,
                                                                             direction == "r_se" ~ 90:180,
                                                                             direction == "r_sw" ~ 180:270,
                                                                             direction == "r_nw" ~ 270:360),
                                                            radius*1852*scale_radii) %>% # 1 knot is 1.852 Km
                                 rbind(data.frame(lon=lon, lat=lat)) %>% 
                                 rename(x=lon, y=lat)
                         }
                         
                         df <-
                             data %>%
                             dplyr::select(r_ne, r_nw, r_se, r_sw) %>%
                             tidyr::gather(direction, radius) 
                         
                         grid <-
                             purrr::pmap(list(df$direction, df$radius), new_arc) %>%
                             dplyr::bind_rows()
                         
                         return(grid)
                     },
                     required_aes = c("x", "y","r_ne", "r_se", "r_nw", "r_sw")
    )

#' Title
#'
#' @param mapping 
#' @param data 
#' @param geom 
#' @param position 
#' @param na.rm 
#' @param show.legend 
#' @param inherit.aes 
#' @param scale_radii 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
stat_hurricane <- function(mapping = NULL, data = NULL, geom = "polygon", 
                           position = "identity", na.rm = FALSE, show.legend = NA, 
                           inherit.aes = TRUE, scale_radii = 1, ...) {
    ggplot2::layer(
        stat = StatHurricane, data = data, mapping = mapping, geom = geom, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(scale_radii = scale_radii, na.rm = na.rm, ...)
    )
}


## Now we Create Geom Hurricane 

geomHurricane <-
    ggplot2::ggproto("GeomPolygon", ggplot2::GeomPolygon,
                     default_aes = ggplot2::aes(colour = "green", 
                                                fill = NA, linetype = 1, 
                                                size = 1, alpha = 0.6)
    )

#' Title
#'
#' @param mapping 
#' @param data 
#' @param position 
#' @param na.rm 
#' @param show.legend 
#' @param inherit.aes 
#' @param scale_radii 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
geom_hurricane <- function(mapping = NULL, data = NULL,
                           position = "identity", na.rm = FALSE, 
                           show.legend = NA, inherit.aes = TRUE, 
                           scale_radii = 1.0, ...) {
    
    ggplot2::layer(stat = StatHurricane, geom = geomHurricane,
                   data = data, mapping = mapping, position = position,
                   show.legend = show.legend, inherit.aes = inherit.aes,
                   params = list(scale_radii = scale_radii, na.rm = na.rm, ...)
    )
}
# List relevant packages
packages <- c('readr', 'dplyr', 'stringr', 'tidyr', 'ggmap','lubridate',
              'geosphere')

# Load packages
lapply(packages, require, character.only = TRUE)


### Here we will read the Hurricane file
data <- read_hurricane("ebtrk_atlc_1988_2015.txt")

## Getting the clean data frame

data_clean <- tidy_hurricane(data)
IKE <- Hurricane(data_clean,storm_year ="IKE-2008",time = "2008-09-12 06:00:00")


## Here will test the code for 
ggplot(data = IKE) +
    geom_hurricane(aes(x = longitude, y = latitude,
                       r_ne = ne, r_se = se, r_nw = nw, r_sw = sw,
                       fill = wind_speed, color = wind_speed)) +
    scale_color_manual(name = "Wind speed (kts)",
                       values = c("red", "orange", "yellow")) +
    scale_fill_manual(name = "Wind speed (kts)",
                      values = c("red", "orange", "yellow"))


## You need to add the API key for ggmap here ###

map_data <- get_map("Louisiana", zoom = 6, maptype = "toner-background")
base_map <- ggmap(map_data, extent = "device")

base_map +
    geom_hurricane(data = IKE,
                   aes(x = longitude, y = latitude,
                       r_ne = ne, r_se = se, r_nw = nw, r_sw = sw,
                       fill = wind_speed, color = wind_speed), scale_radii = 0.5) +
    scale_color_manual(name = "Wind speed (kts)",
                       values = c("red", "orange", "yellow")) +
    scale_fill_manual(name = "Wind speed (kts)",
                      values = c("red", "orange", "yellow"))