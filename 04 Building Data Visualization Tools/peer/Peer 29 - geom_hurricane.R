#' accessing data from hurracanes
#' 
#' @importFrom readr read.fwf fwf_widths
#'
#' @param hurracanes file name
#' 
#' @example 
#' \dontrun{
#'   load_hdata("data/ebtrk_atlc_1988_2015.txt")
#' }
#' 
#' @export
load_hdata <- function(filename) {
  
  # from coursera instructions given
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
                                fwf_widths(ext_tracks_widths, ext_tracks_colnames),
                                na = "-99")
  
  return(ext_tracks)
  
}


#' Hurricane data sorted by criteria
#' \donotrun{data_hurricane_tidy} Clear Hurricane Data
#' 
#' @importFrom stringr str_c str_to_title
#' @importFrom dplyr mutate_ select_
#' @importFrom tidyr gather spread
#' 
#' @param data data to tidy
#' 
#' @export
data_hurricane_tidy <- function(data) {
  data %>%
    dplyr::mutate_(storm_id = ~stringr::str_c(stringr::str_to_title(storm_name), year, sep = '-'),
                   date = ~stringr::str_c(year, '-', month, '-', day, ' ', hour, ':', '00', ':', '00'),
                   longitude = ~-longitude
    ) %>%
    dplyr::select_(.dots = c('storm_id', 'date', 'longitude', 'latitude', 
                             'radius_34_ne', 'radius_34_se', 'radius_34_sw', 'radius_34_nw',
                             'radius_50_ne', 'radius_50_se', 'radius_50_sw', 'radius_50_nw',
                             'radius_64_ne', 'radius_64_se', 'radius_64_sw', 'radius_64_nw')
    ) %>%
    tidyr::gather(variable, value, -storm_id, -date,-latitude, -longitude, -storm_id, -date) %>% mutate_(wind_speed = ~str_extract(variable, "(34|50|64)"),
                                                                                                         variable = ~str_extract(variable, "(ne|nw|se|sw)")) %>% tidyr::spread(variable, value) %>% select_(.dots = c('storm_id', 'date', 'latitude', 'longitude', 'wind_speed', 'ne', 'nw', 'se', 'sw'))
  
  
}

#' filter data by time and name according to hurray
#' 
#' @importFrom dplyr filter_
#' 
#' @param data input data to filter
#' @param hurricane id to filter the data by
#' @param observation time to filter the data by
#' 
#' @export
filter_hurricane_tidy <- function(data, hurricane, observation) {
  data <- filter_(data, ~storm_id == hurricane & date == observation)
  
}


#' Function for the proto geom hurricane
#' 
#' @importFrom ggplot2 layer
#' 
#' @param mapping Set of aesthetic mappings created by aes or aes_. If specified and inherit.aes = TRUE (the default), it is combined with the default mapping at the top level of the plot. You must supply mapping if there is no plot mapping.
#' @param data The data to be displayed in this layer. There are three options: If NULL, the default, the data is inherited from the plot data as specified in the call to ggplot. A data.frame, or other object, will override the plot data. All objects will be fortified to produce a data frame. See fortify for which variables will be created. A function will be called with a single argument, the plot data. The return value must be a data.frame., and will be used as the layer data.
#' @param stat The statistical transformation to use on the data for this layer, as a string.
#' @param position Position adjustment, either as a string, or the result of a call to a position adjustment function.
#' @param na.rm If FALSE, the default, missing values are removed with a warning. If TRUE, missing values are silently removed.
#' @param show.legend logical. Should this layer be included in the legends? NA, the default, includes if any aesthetics are mapped. FALSE never includes, and TRUE always includes.
#' @param inherit.aes If FALSE, overrides the default aesthetics, rather than combining with them. This is most useful for helper functions that define both data and aesthetics and shouldn't inherit behaviour from the default plot specification, e.g. borders.
#' 
#' @export
geom_hurricane <- function(mapping = NULL, data = NULL, stat = 'identity',
                           position = 'identity', na.rm = FALSE,
                           show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = geom_hurricane, mapping = mapping,
    data = data, stat = stat, position = position, 
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
    
  )
}


#' Function to build a new class for geom_hurricane
#' 
#' @importFrom ggplot2 ggproto
#' @importFrom base data.frame as.character
#' @importFrom dplyr bind_rows rename_ mutate_
#' @importFrom grid polygonGrob gpar 
#' 
#' @param required_aes required aesthetic arguments for the geom_hurricane supplied in character vector
#' @param default_aes default values for aesthetic arguments
#' @param draw_key the function to draw the legend with the associated geom
#' @param draw_group where the bulk of this geom is constructed
#' @export
geom_hurricane <- ggplot2::ggproto("geom_hurricane", Geom,
                                         required_aes = c("x", "y",
                                                          "r_ne", "r_se", "r_nw", "r_sw"
                                         ),
                                         default_aes = aes(fill = 1, colour = 1, alpha = 1, scale_radii = 1),
                                         draw_key = draw_key_polygon,
                                         draw_group = function(data, panel_scales, coord) {
                                           
                                           coords <- coord$transform(data, panel_scales)
                                           
                                           data <- data %>% mutate_(r_ne = ~r_ne*1852*scale_radii,
                                                                    r_se = ~r_se*1852*scale_radii,
                                                                    r_sw = ~r_sw*1852*scale_radii,
                                                                    r_nw = ~r_nw*1852*scale_radii
                                           )
                                           
                                           
                                           for (i in 1:nrow(data)) {
                                             
                                             df_nw <- base::data.frame(colour = data[i,]$colour,
                                                                       fill = data[i,]$fill,
                                                                       geosphere::destPoint(p = c(data[i,]$x, data[i,]$y),
                                                                                            b = 270:360,
                                                                                            d = data[i,]$r_nw),
                                                                       group = data[i,]$group,
                                                                       PANEL = data[i,]$PANEL,
                                                                       alpha = data[i,]$alpha
                                             )
                                             
                                             df_ne <- base::data.frame(colour = data[i,]$colour,
                                                                       fill = data[i,]$fill,
                                                                       geosphere::destPoint(p = c(data[i,]$x, data[i,]$y),
                                                                                            b = 1:90,
                                                                                            d = data[i,]$r_ne),
                                                                       group = data[i,]$group,
                                                                       PANEL = data[i,]$PANEL,
                                                                       alpha = data[i,]$alpha
                                             )
                                             
                                             df_se <- base::data.frame(colour = data[i,]$colour,
                                                                       fill = data[i,]$fill,
                                                                       geosphere::destPoint(p = c(data[i,]$x, data[i,]$y),
                                                                                            b = 90:180,
                                                                                            d = data[i,]$r_se),
                                                                       group = data[i,]$group,
                                                                       PANEL = data[i,]$PANEL,
                                                                       alpha = data[i,]$alpha
                                             )
                                             
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
                                           
                                           
                                           df_points <- df_points %>% dplyr::rename_('x' = 'lon',
                                                                                     'y' = 'lat'
                                           )
                                           
                                           df_points$colour <- base::as.character(df_points$colour)
                                           df_points$fill <- base::as.character(df_points$fill)
                                           

                                           coords_df <- coord$transform(df_points, panel_scales)
                                           
                                           grid::polygonGrob(
                                             x= coords_df$x,
                                             y = coords_df$y,
                                             gp = grid::gpar(col = coords_df$colour, fill = coords_df$fill, alpha = coords_df$alpha)
                                           )
                                           
                                         }
                                         
)





# Listing relevant packages
packages <- c('readr', 'dplyr', 'stringr', 'tidyr', 'ggmap', 'geosphere')

# Loading packages
lapply(packages, require, character.only = TRUE)


# Reading data in R
storm_observation <- load_hdata('ebtrk_atlc_1988_2015.txt') %>% 
  data_hurricane_tidy() %>% 
  filter_hurricane_tidy(hurricane = 'Ike-2008', observation = '2008-09-13 12:00:00')



# Create map and add hurricane Ike storm observation
maping_ike <- get_map("Louisiana", zoom = 6, maptype = "hybrid") %>%
  ggmap(extent = "device") +
  geom_hurricane(data = storm_observation,
                 aes(x = longitude, y = latitude, 
                     r_ne = ne, r_se = se, r_nw = nw, r_sw = sw,
                     fill = wind_speed, color = wind_speed)) + 
  scale_color_manual(name = "Wind speed (kts)", 
                     values = c("red", "orange", "yellow")) + 
  scale_fill_manual(name = "Wind speed (kts)", 
                    values = c("red", "orange", "yellow"))

get_map("Louisiana", zoom = 6, maptype = "toner-background") %>%
  ggmap(extent = "device") +
  geom_hurricane(data = storm_observation,
                 aes(x = longitude, y = latitude, 
                     r_ne = ne, r_se = se, r_nw = nw, r_sw = sw,
                     fill = wind_speed, color = wind_speed)) + 
  scale_color_manual(name = "Wind speed (kts)", 
                     values = c("red", "orange", "yellow")) + 
  scale_fill_manual(name = "Wind speed (kts)", 
                    values = c("red", "orange", "yellow"))
