# Coursera: Data Visualization Tools Assignment Week 4
# In this project we use a function provided in the assignment to read the data.
# Then we had to put it in long format then save the result in a txt file
# To use get_map you need to get a key form google for their API. 
# To use this script load the cleand data and replace it on read_csv function.

### Build the Hurricane Geom
#Load the cleaned data
library(data.table)
library(lubridate)
data_hurricane <- read_csv(file = "Data_cleaned.txt")
setDT(data_hurricane)
#Get one observation
data_hurricane[, date := ymd_hms(date)]
storm_observation <-
  data_hurricane[storm_id == "KATRINA-2005" &
                   date == ymd_hms("2005-08-29 12:00:00"),]
storm_observation[, wind_speed := as.factor(wind_speed)]
# Try another Filter over Ike hurricane
Ike_2008 = data_hurricane[storm_id == "IKE-2008" &
                            date == ymd_hms("2008-09-13 12:00:00"), ]
Ike_2008[, wind_speed := as.factor(wind_speed)]

#' Create 'geom_hurricane_proto' class
#'
#' We use ggproto function to create a new geom class 'geom_hurricane_proto'
#'
#' @param required_aes required aesthetic arguments for the geom_hurricane supplied in character vector
#' @param default_aes default values for aesthetic arguments
#' @param draw_key the function to draw the legend with the associated geom
#' @param draw_group where the bulk of this geom is constructed
#'
#' @importFrom ggplot2 ggproto
#' @importFrom base data.frame as.character
#' @importFrom dplyr bind_rows rename_ mutate_
#' @importFrom grid polygonGrob gpar
#'
#' @return a geom class that can be used to add the hurricane wind radii chart for a single storm observation to a map
#'
#' @examples geom_hurricane_proto
#'
#' @export
library(grid)
geom_hurricane_proto <-
  ggplot2::ggproto(
    "geom_hurricane_proto",
    Geom,
    required_aes = c("x", "y",
                     "r_ne", "r_se", "r_nw", "r_sw"),
    default_aes = aes(
      fill = 1,
      colour = 1,
      alpha = 1,
      scale_radii = 1
    ),
    draw_key = draw_key_polygon,
    draw_group = function(data, panel_scales, coord) {
      ## Transform the data first
      coords <-
        coord$transform(data, panel_scales)
      
      # Convert nautical miles to meters and multiply by scale factor
      data <-
        data %>% mutate_(
          r_ne = ~ r_ne * 1609 * scale_radii,
          r_se = ~
            r_se * 1609 * scale_radii,
          r_sw = ~
            r_sw * 1609 * scale_radii,
          r_nw = ~
            r_nw * 1609 * scale_radii
        )
      
      
      # Loop over the data and create the points for each quandrant
      for (i in 1:nrow(data)) {
        # Create the Northwest Quandrant
        df_nw <-
          base::data.frame(
            colour = data[i, ]$colour,
            fill = data[i, ]$fill,
            geosphere::destPoint(
              p = c(data[i, ]$x, data[i, ]$y),
              b = 270:360,
              d = data[i, ]$r_nw
            ),
            group = data[i, ]$group,
            PANEL = data[i, ]$PANEL,
            alpha = data[i, ]$alpha
          )
        
        # Create the Northeast Quandrant
        df_ne <-
          base::data.frame(
            colour = data[i, ]$colour,
            fill = data[i, ]$fill,
            geosphere::destPoint(
              p = c(data[i, ]$x, data[i, ]$y),
              b = 1:90,
              d = data[i, ]$r_ne
            ),
            group = data[i, ]$group,
            PANEL = data[i, ]$PANEL,
            alpha = data[i, ]$alpha
          )
        
        # Create the Southeast Quandrant
        df_se <-
          base::data.frame(
            colour = data[i, ]$colour,
            fill = data[i, ]$fill,
            geosphere::destPoint(
              p = c(data[i, ]$x, data[i, ]$y),
              b = 90:180,
              d = data[i, ]$r_se
            ),
            group = data[i, ]$group,
            PANEL = data[i, ]$PANEL,
            alpha = data[i, ]$alpha
          )
        
        # Create the Southwest Quandrant
        df_sw <-
          data.frame(
            colour = data[i, ]$colour,
            fill = data[i, ]$fill,
            geosphere::destPoint(
              p = c(data[i, ]$x, data[i, ]$y),
              b = 180:270,
              d = data[i, ]$r_sw
            ),
            group = data[i, ]$group,
            PANEL = data[i, ]$PANEL,
            alpha = data[i, ]$alpha
          )
        
        # bind all the rows into a dataframe
        df_points <-
          dplyr::bind_rows(list(df_nw, df_ne, df_se, df_sw))
        
      }
      
      
      # Rename columns x and y from lon and lat repectively
      df_points <-
        df_points %>% dplyr::rename_('x' = 'lon',
                                     'y' = 'lat')
      
      # Convert to character
      df_points$colour <-
        base::as.character(df_points$colour)
      df_points$fill <-
        base::as.character(df_points$fill)
      
      
      ## transform data points
      coords_df <-
        coord$transform(df_points, panel_scales)
      
      ## Construct grid polygon
      grid::polygonGrob(
        x = coords_df$x,
        y = coords_df$y,
        gp = grid::gpar(
          col = coords_df$colour,
          fill = coords_df$fill,
          alpha = coords_df$alpha
        )
      )
      
    }
    
  )

#' Create function that will build a layer based on specified geom
#'
#' With the created geom class, we create the actually function that will build a layer based on your geom specification.
#'
#' @param mapping
#' @param data
#' @param stat
#' @param position
#' @param na.rm
#' @param show.legend
#' @param inherit.aes
#'
#' @importFrom ggplot2 layer
#'
#' @examples geom_hurricane()
#'
#' @retrun a function that will build a layer based on geom_hurricane_proto geom.
#'
#' @export
geom_hurricane <-
  function(mapping = NULL,
           data = NULL,
           stat = 'identity',
           position = 'identity',
           na.rm = FALSE,
           show.legend = NA,
           inherit.aes = TRUE,
           ...) {
    ggplot2::layer(
      geom = geom_hurricane_proto,
      mapping = mapping,
      data = data,
      stat = stat,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, ...)
      
    )
  }


# Display hurricane wind radii chart to a base map
# Important you need an API Key from Google
library(ggmap)
# this sets your google map for this session
# register_google(key = "[your key]")


map_plot <-
  get_map("Lousiana", zoom = 6, maptype = "toner-background")

map_plot %>%
  ggmap(extent = "device") +
  geom_hurricane(
    data = storm_observation,
    aes(
      x = longitude,
      y = latitude,
      r_ne = ne,
      r_se = se,
      r_nw = nw,
      r_sw = sw,
      color = wind_speed,
      fill = wind_speed
    )
  ) +
  scale_color_manual(name = "Wind speed (kts)",
                     values = c("red", "orange", "yellow")) +
  scale_fill_manual(name = "Wind speed (kts)",
                    values = c("red", "orange", "yellow"))


map_plot %>%
  ggmap(extent = "device") +
  geom_hurricane(
    data = storm_observation,
    aes(
      x = longitude,
      y = latitude,
      r_ne = ne,
      r_se = se,
      r_nw = nw,
      r_sw = sw,
      color = wind_speed,
      fill = wind_speed
    ),
    scale_radii = 0.5
  ) +
  scale_color_manual(name = "Wind speed (kts)",
                     values = c("red", "orange", "yellow")) +
  scale_fill_manual(name = "Wind speed (kts)",
                    values = c("red", "orange", "yellow"))
