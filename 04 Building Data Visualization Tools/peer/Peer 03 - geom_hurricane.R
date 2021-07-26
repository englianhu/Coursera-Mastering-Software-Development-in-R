# Hurricane Geom --------------------------------------------------------------------

#' Construct a geom to represent a hurricane wind radii chart.
#' 
#' \code(geom_hurricane) displays a wind radii chart showing the
#' extent of winds of three different intensities (34, 50, 64 knots)
#' from the storm center.
#' 
#' The required aesthetics are the longitude (x), latitude (y), 
#' and the wind radii in each of the four quadrants.
#' 
#' The optional aesthetic scale_radii allows the wind radii to be scaled to a percentage
#' of the maximum wind radii in each quadrant.
#' 
#' @section Aesthetics:
#' \code{geom_hurricane} understands the following aesthetics (required aesthetics are in bold):
#' \itemize{
#'   \item \strong{x}
#'   \item \strong{y}
#'   \item \strong{r_ne}
#'   \item \strong{r_se}
#'   \item \strong{r_nw}
#'   \item \strong{r_sw}
#'   \item color
#'   \item fill
#'   \item linetype
#'   \item alpha
#'   \item scale_radii
#' }
#' 
#' @inheritParams layer
#' 
#' @section Depends on:
#' \enumerate{
#'   \item \code{\link[ggplot2]{layer}} in the \code{ggplot2} package.
#' }
#'
#' @export

GeomHurricane <- ggplot2::ggproto("GeomHurricane",
                                  ggplot2::Geom,
                                  
                                  required_aes = c("x", # longitude
                                                   "y", # latitude
                                                   "r_ne",
                                                   "r_se",
                                                   "r_sw",
                                                   "r_nw"
                                  ),
                                  
                                  default_aes = ggplot2::aes(colour = "black",
                                                             fill = "black",
                                                             linetype = 0,
                                                             alpha = 0.65,
                                                             scale_radii = 1.0
                                  ),
                                  
                                  draw_key = ggplot2::draw_key_polygon,
                                  
                                  draw_group = function(data, panel_scales, coord) {
                                    
                                    # Helper function
                                    polygon_section <- function (data) {
                                      geosphere::destPoint(c(data$x, data$y),
                                                           b = if (data$quadrant == "r_ne") {
                                                             0:90
                                                           } else if (data$quadrant == "r_se") {
                                                             90:180
                                                           } else if (data$quadrant == "r_sw") {
                                                             180:270
                                                           } else {
                                                             270:360
                                                           },
                                                           d = data$radius * 1852
                                      ) %>%
                                        
                                        # Convert output of destPoint from matrix to dataframe to use rbind
                                        as.data.frame() %>% 
                                        
                                        # Rename the columns   
                                        dplyr::rename(x = lon,
                                                      y = lat) %>%
                                        
                                        # Add the center
                                        rbind(data.frame(x = data$x,
                                                         y = data$y)
                                        )  
                                    }
                                    
                                    # Define the polygonal sections in each region for each windspeed, then bind
                                    # Apply scale_radii factor
                                    geom_data <- data %>% 
                                      tidyr::gather(r_ne, r_nw, r_se, r_sw, key = quadrant, value = radius) %>% 
                                      dplyr::mutate(radius = scale_radii * radius) %>% 
                                      split(seq(nrow(.))) %>% 
                                      purrr::map(polygon_section) %>% 
                                      dplyr::bind_rows() %>% 
                                      coord$transform(panel_scales)
                                    
                                    grid::polygonGrob(x = geom_data$x,
                                                      y = geom_data$y,
                                                      default.units = "native",
                                                      gp = grid::gpar(col = data$colour,
                                                                      fill = data$fill,
                                                                      alpha = data$alpha,
                                                                      lty = 1
                                                      )
                                    )
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

# Read in the data --------------------------------------------------------
library(readr)
library(stringr)
library(dplyr)
library(tidyr)
library(purrr)
library(ggmap)
library(ggplot2)
library(grid)
library(png)

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

# Raw Data
ext_tracks <- read_fwf("ebtrk_atlc_1988_2015.txt", 
                       fwf_widths(ext_tracks_widths, ext_tracks_colnames),
                       na = "-99")


# Tidy the Data -----------------------------------------------------------

# This function tidies the dataset
HurriClean <- function(data) {
  
  data %>% 
    
    dplyr::mutate(storm_id = paste0(stringr::str_to_title(storm_name), "-", year),
                  longitude = - longitude,
                  date = as.POSIXct(paste0(year, "-", month, "-", day, " ", hour, ":00:00"))
                  ) %>% 
    
    dplyr::select(storm_id, date, longitude, latitude,
                  radius_34_ne, radius_34_se, radius_34_sw, radius_34_nw,
                  radius_50_ne, radius_50_se, radius_50_sw, radius_50_nw,
                  radius_64_ne, radius_64_se, radius_64_sw, radius_64_nw
                  ) %>% 
    
    tidyr::gather(variable, value, -storm_id, -date, -latitude, -longitude) %>% 
    
    dplyr::mutate(wind_speed = stringr::str_extract(variable, "(34|50|64)"),
                  variable = stringr::str_extract(variable, "(ne|nw|se|sw)")
                  ) %>% 
    
    tidyr::spread(variable, value)
}

# This function extracts a given storm observation from the tidied hurricane data
Hurricane_Subset <- function(data, storm_name, year, split_date = FALSE) {
  
  result <- data %>% 
    dplyr::filter(storm_id == paste0(stringr::str_to_title(storm_name), "-", year)) %>% 
    dplyr::arrange(date)
  
  if (split_date) {
    result <- result %>% 
      split(as.factor(.$date)) %>% 
      purrr::map(as.data.frame)
  }
  
  return(result)
}

# Tidy hurricane dataset
TidyHurricane <- HurriClean(ext_tracks)


# Storm Observations ------------------------------------------------------

# Hurricane Ike
# Full
HurricaneIke <- Hurricane_Subset(TidyHurricane, "Ike", 2008, split = TRUE)
# Observation
Ike_Time <- "2008-09-13 06:00:00"
Ike_Observation <- HurricaneIke[[Ike_Time]]

# Hurricane Katrina
# Full
HurricaneKatrina <- Hurricane_Subset(TidyHurricane, "Katrina", 2005, split = TRUE)
# Observation
Katrina_Observation <- HurricaneKatrina[["2005-08-29 12:00:00"]]



# Hurricane Wind Radii Plot:

png(filename = "Hurricane_Geom_Assignment.png", width = 1536, height = 834)

ggmap::get_map("Louisiana",
               zoom = 6,
               maptype = "toner-background") %>%
  
  ggmap(extent = "device") + 
  
  geom_hurricane(data = Ike_Observation,
                 aes(x = longitude,
                     y = latitude,
                     r_ne = ne,
                     r_se = se,
                     r_nw = nw,
                     r_sw = sw,
                     fill = wind_speed,
                     color = wind_speed,
                     scale_radii = 1.0)
  ) +
  
  scale_color_manual(name = "Wind speed (kts)",
                     values = c("red", "orange", "yellow")
  ) +
  
  scale_fill_manual(name = "Wind speed (kts)",
                    values = c("red", "orange", "yellow")
  ) +
  
  labs(title = paste0("Hurricane Ike: ", Ike_Time))

dev.off()
