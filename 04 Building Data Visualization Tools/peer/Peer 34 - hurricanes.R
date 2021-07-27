#' print("geom_hurricane")
#' This is a ggplot2 geom to build a layer to display the extent of different
#' levels of hurricane wind speeds for each of the quadrants.
#'
#' @inheritParams ggplot2::geom_polygon
#' 
#' @param scale_radii Allows to scale the extent of the data to a fraction of
#'     its real extent.
#'
#' @return This function adds a layer to a ggplot2 object
#'
#' @examples
#' \dontrun{
#' ggplot(data = katrina) +
#' geom_hurricane(aes(x = longitude, y = latitude,
#' r_ne = ne, r_se = se, r_nw = nw, r_sw = sw,
#' fill = wind_speed, color = wind_speed)) +
#' scale_color_manual(name = "Wind speed (kts)",
#' values = c("red", "orange", "yellow")) +
#' scale_fill_manual(name = "Wind speed (kts)",
#' values = c("red", "orange", "yellow"))
#' }
#'
#' @importFrom ggplot2 layer
#' 
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

#' print("GeomHurricane")
#' This is the function that actually does the job behind a call to the ggplot2
#' geom_hurricane.
#'
#' @importFrom ggplot2 ggproto
#' @importFrom geosphere destPoint
#' @importFrom dplyr rename
#' @importFrom grid polygonGorb
GeomHurricane <- ggplot2::ggproto("GeomHurricane", Geom, required_aes = c("x", "y",
                                            "r_ne", "r_se","r_nw","r_sw"),
                default_aes = aes(fill = "black", colour = "black", linetype=0,
                                  alpha = 0.5, scale_radii = 1),
                draw_key = draw_key_polygon,
                draw_group = function(data, panel_scales, coord) {
                        p_df = data.frame()
                        for (i in 1:nrow(data)){
                                p_ne=data.frame(
                                        geosphere::destPoint(p=c(data[i,]$x,
                                                                 data[i,]$y),
                                                             b=0:90,
                                                             d=nm2m(data[i,]$scale_radii*data[i,]$r_ne)),
                                        colour=data[i,]$colour,
                                        fill=data[i,]$fill,
                                        group=data[i,]$group,
                                        PANEL=data[i,]$PANEL,
                                        alpha=data[i,]$alpha)
                                p_se=data.frame(
                                        geosphere::destPoint(p=c(data[i,]$x,
                                                                 data[i,]$y),
                                                             b=90:180,
                                                             d=nm2m(data[i,]$scale_radii*data[i,]$r_se)),
                                        colour=data[i,]$colour,
                                        fill=data[i,]$fill,
                                        group=data[i,]$group,
                                        PANEL=data[i,]$PANEL,
                                        alpha=data[i,]$alpha)
                                p_sw=data.frame(
                                        geosphere::destPoint(p=c(data[i,]$x,
                                                                 data[i,]$y),
                                                             b=180:270,
                                                             d=nm2m(data[i,]$scale_radii*data[i,]$r_sw)),
                                        colour=data[i,]$colour,
                                        fill=data[i,]$fill,
                                        group=data[i,]$group,                             
                                        PANEL=data[i,]$PANEL,
                                        alpha=data[i,]$alpha)
                                p_nw=data.frame(
                                        geosphere::destPoint(p=c(data[i,]$x,
                                                                 data[i,]$y),
                                                             b=270:360,
                                                             d=nm2m(data[i,]$scale_radii*data[i,]$r_nw)),
                                        colour=data[i,]$colour,
                                        fill=data[i,]$fill,
                                        group=data[i,]$group,
                                        PANEL=data[i,]$PANEL,
                                        alpha=data[i,]$alpha)
                                p_df=dplyr::bind_rows(list(p_df,p_nw,p_ne,p_se,p_sw))
                        }
                        
                        p_df = p_df %>% dplyr::rename(x = lon, y = lat)
                        p_df$colour = as.character(p_df$colour)
                        p_df$fill = as.character(p_df$fill)
                        
                        c_df = coord$transform(p_df, panel_scales)

                        grid::polygonGrob(x = c_df$x, y = c_df$y,
                                          gp = grid::gpar(col = c_df$colour,
                                          fill = c_df$fill, alpha = c_df$alpha))
                        })


#' print("nm2m")
#' This function converts nautical miles to meters
#'
#' @param nm Numerica value in nautical miles. It can be a single value or a
#'     numeric vector
#'     
#' @return Value converted in meters
nm2m <- function (nm){
        return(1852*nm)
}

# The execution begins

# Download and unzip data if necessary
file="ebtrk_atlc_1988_2015.txt"
if (!file.exists(file)){
        zipfile="hurricanes.zip"
        if (!file.exists(zipfile)){
                url="https://d18ky98rnyall9.cloudfront.net/_7ed6a595f3e1ac944ccbb1f07db4caae_hurricanes_data.zip?Expires=1606608000&Signature=U570hKU1bqiWd6kg7GzAo2RmW19o-jsTFXjp4zCHNBZBsnc5cRFq9rjZFKNtJfTMqwv~GmHVs9cxCuUEXh3Jc1fLyO17RasFQpcKwNMRwsvE3czBYQieEts9iS11sQhX9BR8jL40HmSVmESKkpB4yU0w7SFXC90Rg4VAbQp-5Lk_&Key-Pair-Id=APKAJLTNE6QMUY6HBC5A"
                download.file(url,zipfile)
        }
        unzip(zipfile)        
}

# Read data
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
library(readr)
ext_tracks <- read_fwf(file, 
                       fwf_widths(ext_tracks_widths, ext_tracks_colnames),
                       na = "-99")

# Data preprocessing and cleaning

library(tidyselect)
library(tidyr)
library(dplyr)
library(Hmisc)
library(lubridate)

data = ext_tracks %>% 
        dplyr::mutate(storm_id=paste(Hmisc::capitalize(tolower(storm_name)),
                                     year,sep="-"),
                      date=paste(paste(year,month,day,sep="-"),
                                 paste(hour,"00","00",sep=":")),
                      longitude = - ifelse(longitude>180,longitude-360,
                                           longitude))  %>% 
        dplyr::select(storm_id,date,latitude,longitude,radius_34_ne,radius_34_se,
                      radius_34_sw,radius_34_nw,radius_50_ne,radius_50_se,
                      radius_50_sw,radius_50_nw,radius_64_ne,radius_64_se,
                      radius_64_sw,radius_64_nw) %>%
        tidyr::pivot_longer(cols = tidyselect::starts_with("radius_"),
                            names_to = c("wind_speed","direction"),
                            names_prefix = "radius_",
                            names_sep = "_", values_to = "radius") %>% 
        tidyr::pivot_wider(names_from = "direction", values_from = "radius") %>%
        dplyr::mutate(wind_speed=wind_speed)

# Extract Katrina and Ike data
katrina = data %>% dplyr::filter(storm_id=="Katrina-2005",
                                 date=="2005-08-29 12:00:00")
ike = data %>% dplyr::filter(storm_id=="Ike-2008",
                             date=="2008-09-13 12:00:00")

# Plotting begins
library(ggmap)
library(grid)
library(geosphere)
library(ggplot2)

get_map(c(ike$longitude[1],ike$latitude[1]), zoom = 6, maptype = "toner-background") %>%
        ggmap(extent = "device") +
        geom_hurricane(data = ike,
                       aes(x = longitude, y = latitude, 
                           r_ne = ne, r_se = se, r_nw = nw, r_sw = sw,
                           fill = wind_speed, color = wind_speed)) + 
        scale_color_manual(name = "Wind speed (kts)", 
                           values = c("red", "orange", "yellow")) + 
        scale_fill_manual(name = "Wind speed (kts)", 
                          values = c("red", "orange", "yellow"))
