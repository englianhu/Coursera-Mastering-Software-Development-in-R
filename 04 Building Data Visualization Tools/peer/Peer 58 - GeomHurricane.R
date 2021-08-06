### Coursera R4 Assignment: Build a New Geom ###

### Questions:
### 1. Build a custom geom for ggplot2 that can be used to add the hurricane wind radii chart
### for a single storm observation to a map (i.e., could be used to recreate the figure shown above).
### 2. Use the geom to map the create a map showing the wind radii chart at one observation times
### for Hurricane Ike, which occurred in September 2008.
### Use an observation time when the storm was near or over the United States.

## Reading in the data:
#Data & Source: Atlantic basin tropical storms since 1988 through the Extended Best Tract dataset
library(readr)
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

ext_tracks <- read_fwf("ebtrk_atlc_1988_2015.txt", 
                       fwf_widths(ext_tracks_widths, ext_tracks_colnames),
                       na = "-99")
#prints:
#-- Column specification -----------------------------------------------------------------
#cols(
#  .default = col_double(),
#  storm_id = col_character(),
#  storm_name = col_character(),
#  month = col_character(),
#  day = col_character(),
#  hour = col_character(),
#  storm_type = col_character(),
#  final = col_character()
#)
# Use `spec()` for the full column specifications.



## Tidying the data and subsetting it to a single observation time (when the storm was near or over the US):
library(dplyr)
library(tidyr)
library(stringr)
dataframe <- ext_tracks %>% 
    dplyr::mutate(storm_id = paste(storm_name, year, sep = '-'),
                  date = paste(year, '-', month, '-', day, ' ', hour, ':', '00', ':', '00'),
                  longitude = -longitude) %>% 
    dplyr::select(c('storm_id', 'date', 'longitude', 'latitude', 
                    'radius_34_ne', 'radius_34_se', 'radius_34_sw', 'radius_34_nw',
                    'radius_50_ne', 'radius_50_se', 'radius_50_sw', 'radius_50_nw',
                    'radius_64_ne', 'radius_64_se', 'radius_64_sw', 'radius_64_nw')) %>%
    tidyr::gather(variable, value, -storm_id, -date,-latitude, -longitude, -storm_id, -date) %>%
    mutate(wind_speed = stringr::str_extract(variable, "(34|50|64)"), variable = str_extract(variable, "(ne|nw|se|sw)")) %>%
    tidyr::spread(variable, value) %>%
    select(c('storm_id', 'date', 'latitude', 'longitude', 'wind_speed', 'ne', 'nw', 'se', 'sw'))
head(dataframe)
#prints:
#A tibble: 6 x 9
#storm_id     date                          latitude longitude wind_speed    ne    nw    se    sw
#  <chr>        <chr>                            <dbl>     <dbl> <chr>      <dbl> <dbl> <dbl> <dbl>
#1 ALBERTO-1988 1988 - 08 - 05   18 : 00 : 00     32       -77.5 34             0     0     0     0
#2 ALBERTO-1988 1988 - 08 - 06   00 : 00 : 00     32.8     -76.2 34             0     0     0     0
#3 ALBERTO-1988 1988 - 08 - 06   06 : 00 : 00     34       -75.2 34             0     0     0     0
#4 ALBERTO-1988 1988 - 08 - 06   12 : 00 : 00     35.2     -74.6 34             0     0     0     0
#5 ALBERTO-1988 1988 - 08 - 06   18 : 00 : 00     37       -73.5 34             0     0     0     0
#6 ALBERTO-1988 1988 - 08 - 07   00 : 00 : 00     38.7     -72.4 34             0     0     0     0



## Filtering for a single Hurricane Ike, which occurred in September 2008 (see hint first)
IKE_data <- dataframe %>%
  filter(storm_id == "IKE-2008")
head(IKE_data)
#prints:
#A tibble: 6 x 9
#storm_id date                          latitude longitude wind_speed    ne    nw    se    sw
#  <chr>    <chr>                            <dbl>     <dbl> <chr>      <dbl> <dbl> <dbl> <dbl>
#1 IKE-2008 2008 - 09 - 01   06 : 00 : 00     17.2     -37   34             0     0     0     0
#2 IKE-2008 2008 - 09 - 01   12 : 00 : 00     17.3     -38.4 34           120    60    75     0
#3 IKE-2008 2008 - 09 - 01   18 : 00 : 00     17.5     -39.9 34           130    75   110     0
#4 IKE-2008 2008 - 09 - 02   00 : 00 : 00     17.8     -41.3 34           140    90   120     0
#5 IKE-2008 2008 - 09 - 02   06 : 00 : 00     18.2     -42.8 34           145   120   120     0
#6 IKE-2008 2008 - 09 - 02   12 : 00 : 00     18.7     -44.3 34           150   120   120     0



## Creating a geom named geom_hurricane()
## and documenting all of its parameters in the oxygen2-style documentation:

#' A new class for the new geom
#' 
#' This is a ggproto function that creates a new class for the new geom, geom_hurricane function.
#' 
#' @param required_aes A character vector of aesthetics needed to render the geom
#' @param default_aes a list generated by aes() of default values for aesthetics
#' @param draw_key renders a single legend key
#' @param draw_panel a function that returns a grid grob to be plotted 
#' 
#' @return a a new class for the new geom, geom_hurricane function, which will be created following this function
#' 
#' @importFrom ggplot2 ggproto 
#' @importFrom grid polygonGrob gpar
#' @importFrom dplyr %>% bind_rows rename
#' @importFrom geosphere destPoint
#' 
#' @export
library(ggplot2)
library(grid)
library(dplyr)
library(geosphere)
GeomHurricane <- ggproto("GeomHurricane", Geom,
                         required_aes = c("x", "y", "ne", "se", "nw", "sw") ,
                         default_aes = aes(fill = 1, colour = 1, alpha = 1, scale_radii = 1),
                         draw_key = draw_key_polygon,
                         draw_panel = function(data, panel_scale, coord){
                           coords <- coord$transform(data, panel_scale)
                           
                           data <- data %>% mutate_(ne = ne*1852*scale_radii,
                                                    se = se*1852*scale_radii,
                                                    sw = sw*1852*scale_radii,
                                                    nw = nw*1852*scale_radii)
                           
                           for (i in 1:nrow(data)) {
                             df_nw <- data.frame(colour = data[i,]$colour,
                                                       fill = data[i,]$fill,
                                                       geosphere::destPoint(p = c(data[i,]$x, data[i,]$y), b = 270:360, d = data[i,]$nw),
                                                       group = data[i,]$group,
                                                       PANEL = data[i,]$PANEL,
                                                       alpha = data[i,]$alpha)
                                                 
                             df_ne <- data.frame(colour = data[i,]$colour,
                                                       fill = data[i,]$fill,
                                                       geosphere::destPoint(p = c(data[i,]$x, data[i,]$y), b = 1:90, d = data[i,]$ne),
                                                       group = data[i,]$group,
                                                       PANEL = data[i,]$PANEL,
                                                       alpha = data[i,]$alpha)
                             
                             df_se <- data.frame(colour = data[i,]$colour,
                                                       fill = data[i,]$fill,
                                                       geosphere::destPoint(p = c(data[i,]$x, data[i,]$y), b = 90:180, d = data[i,]$se),
                                                       group = data[i,]$group,
                                                       PANEL = data[i,]$PANEL,
                                                       alpha = data[i,]$alpha)
                             
                             df_sw <- data.frame(colour = data[i,]$colour,
                                                 fill = data[i,]$fill,
                                                 geosphere::destPoint(p = c(data[i,]$x, data[i,]$y), b = 180:270, d = data[i,]$sw),
                                                 group = data[i,]$group,
                                                 PANEL = data[i,]$PANEL,
                                                 alpha = data[i,]$alpha)
                       

                             df_points <- dplyr::bind_rows(list(df_nw, df_ne, df_se, df_sw))
                           }
                           
                          
                           df_points <- df_points %>% dplyr::rename('x' = 'lon', 'y' = 'lat')
                           df_points$colour <- as.character(df_points$colour)
                           df_points$fill <- as.character(df_points$fill)

                           grid::polygonGrob(x= coords_df$x, y = coords_df$y,
                             gp = grid::gpar(col = coords_df$colour, fill = coords_df$fill, alpha = coords_df$alpha))
                         }
)



#' A new geom which will apply the new class GeomHurricane created above
#' 
#' This is a geom_* function applying the new class GeomHurricane,
#' which returns a layer for the ggproto function of the new class GeomHurricane, to which can be added to a plot created with the ggplot2 function.
#' 
#' @param mapping set of aesthetic mappings created by aes() or aes_(). If specified and inherit.aes = TRUE (the default), it is combined with the default mapping at the top level of the plot. You must supply mapping if there is no plot mapping.
#' @param data the data to be displayed in this layer. There are three options: If NULL, the default, the data is inherited from the plot data as specified in the call to ggplot(). A data.frame, or other object, will override the plot data. All objects will be fortified to produce a data frame. See fortify() for which variables will be created. A function will be called with a single argument, the plot data. The return value must be a data.frame, and will be used as the layer data. A function can be created from a formula (e.g. ~ head(.x, 10)).
#' @param stat the statistical transformation to use on the data for this layer, as a string.
#' @param position position adjustment, either as a string, or the result of a call to a position adjustment function.
#' @param na.rm if FALSE, the default, missing values are removed with a warning. If TRUE, missing values are silently removed.
#' @param show.ledgend logical. Should this layer be included in the legends? NA, the default, includes if any aesthetics are mapped. FALSE never includes, and TRUE always includes. It can also be a named logical vector to finely select the aesthetics to display.
#' @param inherit.aes if FALSE, overrides the default aesthetics, rather than combining with them. This is most useful for helper functions that define both data and aesthetics and shouldn't inherit behaviour from the default plot specification, e.g. borders().
#' 
#' @return a layer for the ggproto function of the new class GeomHurricane
#' 
#' @importFrom ggplot2 layer
#' 
#' @export
library(ggplot2)
geom_hurricane <- function (mapping = NULL, data = NULL, stat = "identity",
                           position = "identity", na.rm = FALSE, 
                           show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomHurricane, mapping = mapping,  
    data = data, stat = stat, position = position, 
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}



## Plotting a wind radii chart of IKE-2008 using the new geom:
ggplot(data = IKE_data) +
  geom_hurricane(aes(x = longitude, y = latitude,
                     ne = ne, se = se, nw = nw, sw = sw,
                     fill = wind_speed, color = wind_speed)) +
  scale_color_manual(name = "Wind speed (kts)",
                     values = c("red", "orange", "yellow")) +
  scale_fill_manual(name = "Wind speed (kts)",
                    values = c("red", "orange", "yellow")) 



## Adding a map that has the wind radii overlayed for Hurricane Ike:
library(ggmap)
get_map("Ike", zoom = 6, maptype = "toner-background") %>%
  ggmap(extent = "device") +
  geom_hurricane(data = IKE_data,
                 aes(x = longitude, y = latitude, 
                     ne = ne, se = se, nw = nw, sw = sw,
                     fill = wind_speed, color = wind_speed)) + 
  scale_color_manual(name = "Wind speed (kts)", 
                     values = c("red", "orange", "yellow")) + 
  scale_fill_manual(name = "Wind speed (kts)", 
                    values = c("red", "orange", "yellow"))
#prints: a map that has the wind radii overlayed for Hurricane Ike
