#' geom_hurricane function for week 4 project
#'
#' @param x longitude
#' @param y latitude
#' @param r_ne Northeast radius
#' @param r_se Southeast radius
#' @param r_sw Southwest radius
#' @param r_nw Northwest radius
#' export
#' examples
#' geom_hurricane(data = ext_tracks_reform, 
#'                 aes(x = longitude, y = latitude,
#'                     r_ne = ne, r_se = se,
#'                     r_nw = nw, r_sw = sw,
#'                     fill = wind_speed,
#'                     color = wind_speed))



GeomHurricane <- ggplot2::ggproto("GeomHurricane",
                                  Geom,
                                  required_aes = c("x",  # longitude
                                                   "y",     # latitude
                                                   "r_ne",  
                                                   "r_se",  
                                                   "r_sw",  
                                                   "r_nw"), 
                                  
                                  default_aes = ggplot2::aes(colour  = "black",  
                                                             fill        = "black",  
                                                             linetype    = 0,        
                                                             alpha       = 0.65,     
                                                             scale_radii = 1.0),     
                                  
                                  draw_key = draw_key_polygon,
                                  
                                  draw_group = function(data,
                                                        panel_scales,
                                                        coord) {
                                      # Creating a data frame
                                      storm_df <- dplyr::as_tibble()
                                      center   <- dplyr::as_tibble()
                                      
                                      
                                      data %>% dplyr::mutate(fill = fill,     
                                                             colour = colour) 
                                      
                                      # Center of the hurricane
                                      center <- data %>% dplyr::select(lon = x,           
                                                                       lat = y)  
                                      
                                      # transform radius
                                      radius <- data %>% dplyr::select(r_ne,       
                                                             r_se,       
                                                             r_sw,       
                                                             r_nw) %>%   
                                          
                                          dplyr::mutate(r_ne = data$scale_radii * r_ne * 1852,  
                                                        r_se = data$scale_radii * r_se * 1852, 
                                                        r_sw = data$scale_radii * r_sw * 1852, 
                                                        r_nw = data$scale_radii * r_nw * 1852) 
                                      
                                      # data for plot
                                      for (i in 1:4) # ne se sw nw 
                                      {
                                          
                                           for (j in 1:nrow(data)) #34 50 60
                                          {
                                              # Generating the points
                                              storm_df <- geosphere::destPoint(c(x = center[j,1],        
                                                                                     y = center[j,2]),       
                                                                                     b = ((i-1)*90):(90*i),  
                                                                                     d = radius[j,i]) %>%    
                                                  
                                                  rbind(c(x = center[j,1],       
                                                          y = center[j,2])) %>%  
                                                  
                                                  rbind(storm_df)  
                                          }
                                          
                                          
                                          storm_points <- storm_df %>% 
                                              
                                              dplyr::as_tibble() %>% 
                                              
                                              dplyr::rename(x = lon,     
                                                            y = lat) %>%  
                                              
                                              coord$transform(panel_scales)  
                                      }
                                      
                                     
                                      grid::polygonGrob(x = storm_points$x,   
                                                        y = storm_points$y,   
                                                        default.units = "native",
                                                        gp = grid::gpar(col = data$colour,  
                                                                        fill = data$fill,   
                                                                        alpha = data$alpha, 
                                                                        lty = 1,            
                                                                        scale_radii = data$scale_radii))       
                                  }
)


geom_hurricane <- function(mapping = NULL,
                           data = NULL,
                           stat = "identity",
                           position = "identity",
                           na.rm = FALSE,
                           show.legend = NA,
                           inherit.aes = TRUE, ...){
    
    ggplot2::layer(geom = GeomHurricane,
                   mapping = mapping,
                   data = data,
                   stat = stat,
                   position = position,
                   show.legend = show.legend,
                   inherit.aes = inherit.aes,
                   params = list(na.rm = na.rm,...)
    )
}

#-------------------------------------------------------------------------------
# use geom_hurricane to analyze IKE-2008 storm ---
# load package 
library(dplyr)

# read in data ---
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

ext_tracks <- readr::read_fwf("ebtrk_atlc_1851_2019.txt", 
                              readr::fwf_widths(ext_tracks_widths, ext_tracks_colnames))


# reformat Ike data---
ext_tracks <- ext_tracks[ext_tracks$storm_name=="IKE",]

ext_tracks$storm_id <- paste0(ext_tracks$storm_name,"-",ext_tracks$year)
ext_tracks$storm_name <- NULL
ext_tracks$date <- paste0(ext_tracks$year,"-",ext_tracks$month,"-",ext_tracks$day," ",ext_tracks$hour,":00:00")
ext_tracks$day <- ext_tracks$month <- ext_tracks$year <- ext_tracks$hour <- NULL
ext_tracks <- ext_tracks[,c(1,25,2:3,10:21)]
ext_tracks <- ext_tracks[ext_tracks$date=="2008-09-11 18:00:00",]
ext_tracks$longitude <- 0-ext_tracks$longitude

ext_tracks_reform <- ext_tracks%>%tidyr::pivot_longer(cols = contains("radius"), names_to = "wind_speed", 
                                                      values_to = "value") %>%
    tidyr::separate(wind_speed, c(NA, "wind_speed", "direction"), sep = "_") %>%
    tidyr::pivot_wider(names_from = "direction", values_from = "value") 

write.table(ext_tracks_reform,"hurricane_IKE_091118.txt",col.names = TRUE, row.names = FALSE,
            sep = "\t", quote = FALSE)



# plot ---
library(ggplot2)
library(ggmap)

# API Key
# how to get API key https://developers.google.com/maps/documentation/maps-static/get-api-key#get-key
register_google(key = "AIzaSyBIlNNE1TkbzMxy2Oz1lOkNzfpCEbOHjJY")

# Google Maps/Stratmen
base_map <- get_map(location = c(-88.9,25.8), # c(longitude,latitude)
                    zoom = 5,
                    maptype = "toner-background") %>%
    
    # Saving the map
    ggmap(extent = "device")



base_map +
    geom_hurricane(data = ext_tracks_reform, aes(x = longitude, y = latitude,
                                                 r_ne = ne, r_se = se,
                                                 r_nw = nw, r_sw = sw,
                                                 fill = wind_speed,
                                                 color = wind_speed)) +
    scale_color_manual(name = "Wind speed (kts)",
                       values = c("red", "orange", "yellow")) +
    scale_fill_manual(name = "Wind speed (kts)",
                      values = c("red", "orange", "yellow"))












