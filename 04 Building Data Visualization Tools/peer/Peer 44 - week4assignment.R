setwd("~/Desktop/Building Data Visualization Tools_Coursera")
library("tidyverse")
library("lubridate")
library(ggmap)
register_google(key = "AIzaSyD6yCp8AzdVOzOMTImY1DjEohPf0e37yMM")

# read in data
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

ext_tracks <- read_fwf("./data/ebtrk_atlc_1988_2015.txt", 
                       fwf_widths(ext_tracks_widths, ext_tracks_colnames),
                       na = "-99")

# clean data
ext_tracks$month <- as.numeric(ext_tracks$month)
ext_tracks$day <- as.numeric(ext_tracks$day)
ext_tracks$hour <- as.numeric(ext_tracks$hour)
ext_tracks$date <- make_datetime(  year = ext_tracks$year,
                                   month = ext_tracks$month,
                                   day = ext_tracks$day,
                                   hour = ext_tracks$hour,
                                   min = 0L,
                                   sec = 0)

ne <- 
  ext_tracks %>% 
  unite("storm_name", "year", col="storm_id", sep="-") %>%
  gather("radius_34_ne", "radius_50_ne", "radius_64_ne", key="radius", value="ne") %>%
  mutate("wind_speed" = parse_number(`radius`)) %>%
  select(storm_id, date, latitude, longitude, wind_speed, ne)

se <- 
  ext_tracks %>% 
  unite("storm_name", "year", col="storm_id", sep="-") %>%
  gather("radius_34_se", "radius_50_se", "radius_64_se", key="radius", value="se") %>%
  mutate("wind_speed" = parse_number(`radius`)) %>%
  select(storm_id, date, latitude, longitude, wind_speed, se)

sw <- 
  ext_tracks %>% 
  unite("storm_name", "year", col="storm_id", sep="-") %>%
  gather("radius_34_sw", "radius_50_sw", "radius_64_sw", key="radius", value="sw") %>%
  mutate("wind_speed" = parse_number(`radius`)) %>%
  select(storm_id, date, latitude, longitude, wind_speed, sw)

nw <- 
  ext_tracks %>% 
  unite("storm_name", "year", col="storm_id", sep="-") %>%
  gather("radius_34_nw", "radius_50_nw", "radius_64_nw", key="radius", value="nw") %>%
  mutate("wind_speed" = parse_number(`radius`)) %>%
  select(storm_id, date, latitude, longitude, wind_speed, nw)

ext_tracks_tidy <-
  ne %>% 
  full_join(se, by=c("storm_id", "date", "latitude", "longitude", "wind_speed")) %>%
  full_join(sw, by=c("storm_id", "date", "latitude", "longitude", "wind_speed")) %>%
  full_join(nw, by=c("storm_id", "date", "latitude", "longitude", "wind_speed")) %>%
  mutate(longitude = - longitude) %>%
  mutate(wind_speed = as.factor(wind_speed)) %>%
  arrange(storm_id, date, wind_speed)


# get data for Katrina on 2005-08-29 12:00:00
katrina <- ext_tracks_tidy %>% filter(storm_id == "KATRINA-2005", date == ymd_hms('2005-08-29 12:00:00') )


# write a new geom for hurricane
geomHurricane <- ggplot2::ggproto(
  "geomHurricane",
  Geom,
  required_aes = c("x", "y", "r_ne", "r_se", "r_nw", "r_sw"),
  default_aes = aes(
    fill = 1,
    color = 1,
    alpha = 0.65,
    scale_radii = 1
  ),
  draw_key = draw_key_polygon,
  draw_group = function(data, panel_scales, coord) {
    # convert nautical miles to meters and multiply by scale_radii
    data <- data %>%
      mutate(
        r_ne = r_ne * 1852 * scale_radii,
        r_se = r_se * 1852 * scale_radii,
        r_sw = r_sw * 1852 * scale_radii,
        r_nw = r_nw * 1852 * scale_radii
      )
    
    # loop over all three wind speeds
    for (i in 1:nrow(data)) {
      l_ne <-
        data.frame(
          geosphere::destPoint(
            p = c(data[i,]$x, data[i,]$y),
            b = 1:90,
            d = data[i,]$r_ne
          ),
          color = data[i,]$colour,
          fill = data[i,]$fill,
          alpha = data[i,]$alpha
        )
      
      l_se <-
        data.frame(
          geosphere::destPoint(
            p = c(data[i,]$x, data[i,]$y),
            b = 90:180,
            d = data[i,]$r_se
          ),
          color = data[i,]$colour,
          fill = data[i,]$fill,
          alpha = data[i,]$alpha
        )
      
      l_sw <-
        data.frame(
          geosphere::destPoint(
            p = c(data[i,]$x, data[i,]$y),
            b = 180:270,
            d = data[i,]$r_sw
          ),
          color = data[i,]$colour,
          fill = data[i,]$fill,
          alpha = data[i,]$alpha
        )
      
      l_nw <-
        data.frame(
          geosphere::destPoint(
            p = c(data[i,]$x, data[i,]$y),
            b = 270:360,
            d = data[i,]$r_nw
          ),
          color = data[i,]$colour,
          fill = data[i,]$fill,
          alpha = data[i,]$alpha
        )
      
      # bind all four quadrants
      l_total <-
        dplyr::bind_rows(l_nw, l_ne, l_se, l_sw)
      
    }
    
    
    # Rename columns to x and y, and convert color and fill as character format
    l_total <- l_total %>%
      dplyr::rename('x' = 'lon', 'y' = 'lat') %>%
      dplyr::mutate(color = as.character(l_total$color),
                    fill = as.character(l_total$fill))
    
    # transform data points
    l_total <-
      coord$transform(l_total, panel_scales)
    
    # grid polygon grob
    grid::polygonGrob(
      x = l_total$x,
      y = l_total$y,
      gp = grid::gpar(
        col = l_total$color,
        fill = l_total$fill,
        alpha = l_total$alpha
      )
    )
    
  }
)


#' geom_hurricane is to build a new geom for plotting huriccane 
#'
#' @param mapping 	Set of aesthetic mappings created by aes() or aes_().
#' @param data The data to be displayed in this layer.
#' @param stat The statistical transformation to use on the data for this layer, as a string.
#' @param position Position adjustment, either as a string, or the result of a call to a position adjustment function.
#' @param na.rm If FALSE, the default, missing values are removed with a warning. If TRUE, missing values are silently removed.
#' @param show.legend logical. Should this layer be included in the legends? NA, the default, includes if any aesthetics are mapped. FALSE never includes, and TRUE always includes. It can also be a named logical vector to finely select the aesthetics to display.
#' @param inherit.aes If FALSE, overrides the default aesthetics, rather than combining with them. This is most useful for helper functions that define both data and aesthetics and shouldn't inherit behaviour from the default plot specification, e.g. borders().
#'
#' @return
#' @export
#'
#' @examples 
#' geom_hurricane(data = katrina, aes(x = longitude, y = latitude, r_ne = ne, r_se = se, r_nw = nw, r_sw = sw, fill = wind_speed, color = wind_speed))

geom_hurricane <- function(mapping = NULL, data = NULL, stat = "identity",
                             position = "identity", na.rm = FALSE, 
                             show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = geomHurricane, mapping = mapping,  
    data = data, stat = stat, position = position, 
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


# plot the map with Katrina hurricane
get_map("Louisiana", zoom = 6, maptype = "toner-background") %>%
  ggmap(extent = "device") +
  geom_hurricane(data = katrina,
                 aes(x = longitude, y = latitude, 
                     r_ne = ne, r_se = se, r_nw = nw, r_sw = sw,
                     fill = wind_speed, color = wind_speed)) + 
  scale_color_manual(name = "Wind speed (kts)", 
                     values = c("red", "orange", "yellow")) + 
  scale_fill_manual(name = "Wind speed (kts)", 
                    values = c("red", "orange", "yellow"))
