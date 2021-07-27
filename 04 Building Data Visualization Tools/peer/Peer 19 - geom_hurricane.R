library(readr)
library(dplyr)
library(stringr)
#' Load hurricane data from provided file.
#' 
#' @importFrom readr read.fwf fwf_widths
#'
#' @param filename Path to a file containing hurricane data
#' 
#' @example 
#' \dontrun{
#'   load_hurricanes("ebtrk_atlc_1988_2015.txt")
#' }
#' 
#' @export
load_hurricanes = function(filename) {
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
  return(ext_tracks)
}

#Auxilary function to tidy data
get_wind = function(data, spd){ 
  data %>%
  select(storm_id, date, latitude, longitude, starts_with(paste0("radius_",spd))) %>%
  rename(ne = !!as.name(paste0("radius_",spd,"_ne")), se = !!as.name(paste0("radius_",spd,"_se")), 
         nw = !!as.name(paste0("radius_",spd,"_nw")), sw = !!as.name(paste0("radius_",spd,"_sw"))) %>%
  mutate(wind_speed = spd)
}

#' Tidy hurricane data according to assignment criteria
#' 
#' 
#' @param data Hurricane data
#' 
#' @export
tidy_hurricanes <- function(data) {
  tmp = data %>% 
    # Create storm_id and date 
    dplyr::mutate(storm_id = paste(stringr::str_to_title(storm_name), year, sep = '-'),
                   date = paste(year, '-', month, '-', day, ' ', hour, ':', '00', ':', '00',sep = ""),
                   longitude = -longitude
    ) %>%
    # Select needed data
    dplyr::select(c('storm_id', 'date', 'longitude', 'latitude',
                             'radius_34_ne', 'radius_34_se', 'radius_34_sw', 'radius_34_nw',
                             'radius_50_ne', 'radius_50_se', 'radius_50_sw', 'radius_50_nw',
                             'radius_64_ne', 'radius_64_se', 'radius_64_sw', 'radius_64_nw')
    )
  
rbind(get_wind(tmp,34),get_wind(tmp,50),get_wind(tmp,64)) %>% arrange(storm_id, date) 
  
}

#' Extract given hurricane and observation time from data
#' 
#' 
#' @param data Input data
#' @param hurricane Hurricane id
#' @param observation Time of observartion
#' 
#' @export
filter_hurricane <- function(data, hurricane, observation) {
  data <- filter(data, storm_id == hurricane & date == observation)
}

#' Function to construct a new class for the geom_hurricane, this function will create a wind radii for given obseravtion.
#' 
#' @param required_aes Required aesthetic arguments
#' @param default_aes Default values for aesthetic arguments
#' @param draw_key Function to draw the legend with the associated geom
#' 
#' @examples
#' \dontrun{
#'   geom_hurricane(data = observation, aes(x = longitude, y = latitude, r_ne = ne, r_se = se, r_nw = nw, r_sw = sw, 
#'   fill = as.factor(wind_speed), color = as.factor(wind_speed))
#' }
#' 
#' @export
geom_hurricane_proto <- ggplot2::ggproto("geom_hurricane_proto", Geom,
                                         required_aes = c("x", "y",
                                                          "r_ne", "r_se", "r_nw", "r_sw"
                                         ),
                                         default_aes = aes(fill = 1, colour = 1, alpha = 1, scale_radii = 1),
                                         draw_key = draw_key_polygon,
                                         draw_group = function(data, panel_scales, coord) {
                                           
                                           coords <- coord$transform(data, panel_scales)
                                           
                                           # Convert nautical miles to meters and multiply by scale factor
                                           data <- data %>% mutate_(r_ne = ~r_ne*1852*scale_radii,
                                                                    r_se = ~r_se*1852*scale_radii,
                                                                    r_sw = ~r_sw*1852*scale_radii,
                                                                    r_nw = ~r_nw*1852*scale_radii
                                           )
                                           
                                           # Loop over the data and create the points for each quandrant
                                           for (i in 1:nrow(data)) {
                                             
                                             # NW
                                             df_nw <- base::data.frame(colour = data[i,]$colour,
                                                                       fill = data[i,]$fill,
                                                                       geosphere::destPoint(p = c(data[i,]$x, data[i,]$y),
                                                                                            b = 270:360,
                                                                                            d = data[i,]$r_nw),
                                                                       group = data[i,]$group,
                                                                       PANEL = data[i,]$PANEL,
                                                                       alpha = data[i,]$alpha
                                             )
                                             
                                             # NE
                                             df_ne <- base::data.frame(colour = data[i,]$colour,
                                                                       fill = data[i,]$fill,
                                                                       geosphere::destPoint(p = c(data[i,]$x, data[i,]$y),
                                                                                            b = 1:90,
                                                                                            d = data[i,]$r_ne),
                                                                       group = data[i,]$group,
                                                                       PANEL = data[i,]$PANEL,
                                                                       alpha = data[i,]$alpha
                                             )
                                             
                                             # SE
                                             df_se <- base::data.frame(colour = data[i,]$colour,
                                                                       fill = data[i,]$fill,
                                                                       geosphere::destPoint(p = c(data[i,]$x, data[i,]$y),
                                                                                            b = 90:180,
                                                                                            d = data[i,]$r_se),
                                                                       group = data[i,]$group,
                                                                       PANEL = data[i,]$PANEL,
                                                                       alpha = data[i,]$alpha
                                             )
                                             
                                             # SW
                                             df_sw <- data.frame(colour = data[i,]$colour,
                                                                 fill = data[i,]$fill,
                                                                 geosphere::destPoint(p = c(data[i,]$x, data[i,]$y),
                                                                                      b = 180:270,
                                                                                      d = data[i,]$r_sw),
                                                                 group = data[i,]$group,
                                                                 PANEL = data[i,]$PANEL,
                                                                 alpha = data[i,]$alpha
                                             )
                                             
                                             df_points <- dplyr::bind_rows(list(df_nw, df_ne, df_se, df_sw))
                                             
                                           }
                                           
                                           # Rename columns x and y from lon and lat repectively
                                           df_points <- df_points %>% dplyr::rename('x' = 'lon',
                                                                                     'y' = 'lat'
                                           )
                                           
                                           # Convert to character
                                           df_points$colour <- base::as.character(df_points$colour)
                                           df_points$fill <- base::as.character(df_points$fill)
                                           coords_df <- coord$transform(df_points, panel_scales)
                                           
                                           ## Construct grid polygon
                                           grid::polygonGrob(
                                             x= coords_df$x,
                                             y = coords_df$y,
                                             gp = grid::gpar(col = coords_df$colour, fill = coords_df$fill, alpha = coords_df$alpha)
                                           )
                                           
                                         }
                                         
)

#' geom_hurricane layer for ggplot2
#' 
#' @param mapping Set of aesthetic mappings created by aes or aes_. Inherited from top-level plot if inherit.aes = TRUE
#' @param data Data to be used in the geom. Can be inherited from top-level plot.
#' @param stat The statistical transformation to use on the data for this layer.
#' @param position Position adjustment - string, or the result of a call to a position adjustment function.
#' @param na.rm If TRUE, missing values are silently removed.
#' @param show.legend Logical switch for including legend for the geom.
#' @param inherit.aes If TRUE layer inherits aes from top-level plot.
#' 
#' @export
geom_hurricane <- function(mapping = NULL, data = NULL, stat = 'identity',
                           position = 'identity', na.rm = FALSE,
                           show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = geom_hurricane_proto, mapping = mapping,
    data = data, stat = stat, position = position, 
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
    
  )
}

sapply(c('readr', 'dplyr', 'stringr', 'tidyr', 'ggmap', 'geosphere'), require, character.only = TRUE)

# Read data into R, tidy it and filter it for Ike-2008 to create storm observation
observation <- load_hurricanes('ebtrk_atlc_1988_2015.txt') %>% 
  tidy_hurricanes() %>% 
  filter_hurricane(hurricane = 'Ike-2008', observation = '2008-09-13 12:00:00')

# Create map and add hurricane Ike storm observation
map_ike <- get_map("Louisiana", zoom = 6, maptype = "toner-background") %>%
  ggmap(extent = "device") +
  geom_hurricane(data = observation,
                 aes(x = longitude, y = latitude, 
                     r_ne = ne, r_se = se, r_nw = nw, r_sw = sw,
                     fill = as.factor(wind_speed), color = as.factor(wind_speed),
                     scale_radii=0.5, 
                     alpha=0.8)) +
  scale_color_manual(name = "Wind speed (kts)",
                     values = c("red", "orange", "yellow")) +
  scale_fill_manual(name = "Wind speed (kts)",
                    values = c("red", "orange", "yellow")) +guides(alpha=FALSE)
