#' Building data visualization tools
#' Build a new geom
#'
#' @title loading ext_tracks
#'
#' @description Loading required data to build a new geom assigment.
#'      We can read the data in with the following function.
#'
#' @importFrom readr read_fwf 
#'
#' @param filename file with data from all storms in the Atlantic basin from 1988–2015
#'
#' @return a data frame resulting from the file import named ext_tracks
#'  
#' @example 
#'      loading_hdata("ebtrk_atlc_1988_2015.txt")

loading_ext_tracks <- function(filename) {

    # txt is a delimited file, so we identify the spacing for each col and prepare the apropriate names for it.
    ext_tracks_widths <- c(7, 10, 2, 2, 3, 5, 5, 6, 4, 5, 4, 4, 5, 3, 4, 3, 3, 3, 4, 3, 3, 3, 4, 3, 3, 3, 2, 6, 1)

    ext_tracks_colnames <- c("storm_id", "storm_name", "month", "day","hour", "year", "latitude", "longitude",
                        "max_wind", "min_pressure", "rad_max_wind", "eye_diameter", "pressure_1", "pressure_2",
                        paste("radius_34", c("ne", "se", "sw", "nw"), sep = "_"),
                        paste("radius_50", c("ne", "se", "sw", "nw"), sep = "_"),
                        paste("radius_64", c("ne", "se", "sw", "nw"), sep = "_"),
                        "storm_type", "distance_to_land", "final")

    # note that na values were identified as -99
    ext_tracks <<- readr::read_fwf(filename, fwf_widths(ext_tracks_widths, ext_tracks_colnames), na = "-99")
    
    return(ext_tracks)
    }

#' @title Cleaning ext_tracks
#'  
#' @description Once we have read the data in, we need to clean the data and subset to a single observation time for a single storm.
#' 
#' @importFrom stringr str_c
#' @importFrom stringr str_to_title
#' @importFrom dplyr mutate_
#' @importFrom dplyr select_
#' @importFrom tidyr gather 
#' @importFrom tidyr spread
#' 
#' @param data dataset to clean to meet assigment parameters
#' 
#' @return a data frame with the following columns storm_id, date, latitude, longitude, wind_speed, ne, nw, se and sw
#'
#' @example 
#'      a <- clean_hdata(ext_tracks)

clean_hdata <- function(data) {
    data %>% 
    
    dplyr::mutate_(storm_id = ~stringr::str_c(stringr::str_to_title(storm_name), year, sep = '-'),
                   date = ~stringr::str_c(year, '-', month, '-', day, ' ', hour, ':', '00', ':', '00'),
                   longitude = ~-longitude) %>% 
    
    dplyr::select_(.dots = c('storm_id', 'date', 'longitude', 'latitude', 
                            'radius_34_ne', 'radius_34_se', 'radius_34_sw', 'radius_34_nw',
                            'radius_50_ne', 'radius_50_se', 'radius_50_sw', 'radius_50_nw',
                            'radius_64_ne', 'radius_64_se', 'radius_64_sw', 'radius_64_nw')) %>%
    
   
    tidyr::gather(variable, value, -storm_id, -date,-latitude, -longitude, -storm_id, -date) %>%

    dplyr::mutate_(wind_speed = ~str_extract(variable, "(34|50|64)"), variable = ~str_extract(variable, "(ne|nw|se|sw)")) %>%

    tidyr::spread(variable, value) %>%

    dplyr::select_(.dots = c('storm_id', 'date', 'latitude', 'longitude', 'wind_speed', 'ne', 'nw', 'se', 'sw'))
    }


#' @title select specific hurricane
#'  
#' @description Filter the data by hurricane name and time
#' 
#' @importFrom dplyr filter_
#' 
#' @param data input data
#' @param hurricane id to filter the data by
#' @param observation time to filter the data by
#' 
#' @example
#'  hurricane_hdata(data = a, hurricane = "Alberto-1988", observation = "1988-08-05 18:00:00")
#'  hurricane_hdata(data = a, hurricane = "Ike-2008", observation = "2008-09-13 12:00:00")

hurricane_hdata <- function(data, hurricane, observation) {
  data <- filter_(data, ~storm_id == hurricane & date == observation)
  }


#' @title Geom hurricane
#'  
#' @description Function to build layer for the geom_hurricane proto function
#' 
#' @importFrom ggplot2 layer
#' 
#' @param mapping 
#' @param data 
#' @param stat 
#' @param position 
#' @param na.rm 
#' @param show.legend 
#' @param inherit.aes
#'

geom_hurricane <- function(mapping = NULL, data = NULL, stat = 'identity', position = 'identity', na.rm = FALSE,
                           show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(geom = geom_hurricane_proto, mapping = mapping,
                 data = data, stat = stat, position = position, 
                 show.legend = show.legend, inherit.aes = inherit.aes,
                 params = list(na.rm = na.rm, ...))
  }

#' @title Geom hurricane proto
#'  
#' @description Construct a new class for the geom_hurricane
#' 
#' @importFrom ggplot2 ggproto
#' @importFrom base data.frame as.character
#' @importFrom dplyr bind_rows
#' @importFrom dplyr rename_ 
#' @importFrom dplyr mutate_
#' @importFrom grid polygonGrob
#' @importFrom grid gpar 
#' 
#' @param required_aes aesthetic arguments 
#' @param default_aes defaults for aesthetic arguments
#' @param draw_key the function to draw the legend with the associated geom
#' @param draw_group

geom_hurricane_proto <- ggplot2::ggproto("geom_hurricane_proto", Geom, required_aes = c("x", "y", "r_ne", "r_se", "r_nw", "r_sw"),
                                default_aes = aes(fill = 1, colour = 1, alpha = 1, scale_radii = 1),
                                draw_key = draw_key_polygon, draw_group = function(data, panel_scales, coord) {
                                  
                                  coords <- coord$transform(data, panel_scales)
                                  
                                  data <- data %>% mutate_(r_ne = ~r_ne*1852*scale_radii,
                                                           r_se = ~r_se*1852*scale_radii,
                                                           r_sw = ~r_sw*1852*scale_radii,
                                                           r_nw = ~r_nw*1852*scale_radii)

                                  for (i in 1:nrow(data)) {
                                    
                                    df_nw <- base::data.frame(colour = data[i,]$colour, fill = data[i,]$fill,
                                                        geosphere::destPoint(p = c(data[i,]$x, data[i,]$y),
                                                                             b = 270:360,
                                                                             d = data[i,]$r_nw), group = data[i,]$group, PANEL = data[i,]$PANEL, alpha = data[i,]$alpha)

                                    df_ne <- base::data.frame(colour = data[i,]$colour, fill = data[i,]$fill,
                                                        geosphere::destPoint(p = c(data[i,]$x, data[i,]$y),
                                                                             b = 1:90,
                                                                             d = data[i,]$r_ne), group = data[i,]$group, PANEL = data[i,]$PANEL, alpha = data[i,]$alpha)
                                    
                                    df_se <- base::data.frame(colour = data[i,]$colour, fill = data[i,]$fill,
                                                        geosphere::destPoint(p = c(data[i,]$x, data[i,]$y),
                                                                             b = 90:180,
                                                                             d = data[i,]$r_se), group = data[i,]$group, PANEL = data[i,]$PANEL, alpha = data[i,]$alpha)
                                    
                                    df_sw <- data.frame(colour = data[i,]$colour, fill = data[i,]$fill,
                                                        geosphere::destPoint(p = c(data[i,]$x, data[i,]$y),
                                                                             b = 180:270,
                                                                             d = data[i,]$r_sw), group = data[i,]$group, PANEL = data[i,]$PANEL, alpha = data[i,]$alpha)
                           
                                    df_points <- dplyr::bind_rows(list(df_nw, df_ne, df_se, df_sw))
                                  }

                                  df_points <- df_points %>% dplyr::rename_('x' = 'lon', 'y' = 'lat')

                                  df_points$colour <- base::as.character(df_points$colour)
                                  df_points$fill <- base::as.character(df_points$fill)

                                  coords_df <- coord$transform(df_points, panel_scales)

                                  grid::polygonGrob(x= coords_df$x, y = coords_df$y, gp = grid::gpar(col = coords_df$colour, fill = coords_df$fill, alpha = coords_df$alpha))
                                }
                                )


# 1. Load data into R, tidy it and select hurricane Ike-2008
storm <- loading_ext_tracks('ebtrk_atlc_1988_2015.txt') %>% 
  clean_hdata() %>% 
  hurricane_hdata(hurricane = 'Ike-2008', observation = '2008-09-13 12:00:00')

# 2. Create map and add hurricane Ike-2008 storm
map_storm <- get_googlemap("Louisiana", zoom = 6, maptype = "roadmap") %>%
   ggmap(extent = "device") +
   geom_hurricane(data = storm,
                  aes(x = longitude, y = latitude, 
                      r_ne = ne, r_se = se, r_nw = nw, r_sw = sw,
                      fill = wind_speed, color = wind_speed,scale_radii = 0.5)) + 
   scale_color_manual(name = "Wind speed (kts)", 
                      values = c("red", "orange", "yellow")) + 
   scale_fill_manual(name = "Wind speed (kts)", 
                     values = c("red", "orange", "yellow"))

