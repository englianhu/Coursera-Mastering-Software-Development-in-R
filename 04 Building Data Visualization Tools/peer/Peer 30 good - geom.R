# Loading the Packages
library(geosphere)
library(dplyr)
library(ggplot2)
library(geomhurricane)

# Generating the points
destPoint(c(0,0),          # Center of the hurricane
          b=1:360,         # 360 degrees (a complete circle)
          d=10000) %>%     # radius in meters
  as_tibble() %>%   # Converting the output of destPoint to tibble
  
  # Plotting the results       
  ggplot(aes(x = lon,
             y = lat)) +geom_polygon()

# Generating the points
destPoint(c(0,0),          # Center of the hurricane
          b=1:90,          # 360 degrees (a complete circle)
          d=10000) %>%     # radius in meters
  as_tibble() %>%   # Converting the output of destPoint to tibble
  
  # Plotting the results       
  ggplot(aes(x = lon,
             y = lat)) + geom_polygon(fill = "tomato3")

# Generating the points
destPoint(c(0,0),          # Center of the hurricane
          b=1:90,          # 360 degrees (a complete circle)
          d=10000) %>%     # radius in meters
  
  as_tibble() %>%   # Converting the output of destPoint to tibble
  
  rbind(c(0,0)) %>% # Adding the center
  # Plotting the results       
  ggplot(aes(x = lon,
             y = lat)) + geom_polygon(fill = "tomato3")

# Creating a list
df_example <- list()

# My loop to create the 4 data set
for (i in 1:4)
{
  # Generating the points
  destPoint(c(0, 0),          # Center of the hurricane
            b=((i-1)*90):(90*i),  # 360 degrees (a complete circle)
            d=2000 - 200 * i) %>%   # radius
    
    rbind(c(0, 0)) %>% # Adding center/origins
    
    as_tibble() -> df_example[[i]]
}

# Ploting in ggplot2
ggplot() + 
  geom_polygon(data = df_example[[1]], # First sector NE
               aes(x = lon, y = lat),
               fill = "lightblue") +
  
  geom_polygon(data = df_example[[2]], # Second sector SE
               aes(x = lon, y = lat),
               fill = "tomato3") +
  
  geom_polygon(data = df_example[[3]], # Third sector SW
               aes(x = lon, y = lat),
               fill = "lightgreen") +
  
  geom_polygon(data = df_example[[4]], # Fourth sector NW
               aes(x = lon, y = lat),
               fill = "orange")

# Function geom_beta
geom_beta <- function(data,
                      x = longitude,
                      y = latitude,
                      r_ne = ne,
                      r_se = se,
                      r_nw = nw,
                      r_sw = sw,
                      fill = wind_speed,
                      color = wind_speed)
{
  # Creating a list to allocate the data frames from ne, se, sw, and nw.
  df_example <- list()
  
  # Center of the hurricane
  center <- cbind(data$longitude,
                  data$latitude)
  
  # Storing the speed in data frame
  r_cardinal <- cbind(data$ne,
                      data$se,
                      data$sw,
                      data$nw)
  
  # Loop to create the 4 data set (ne, se, sw, and nw)
  for (i in 1:4)
  {
    # Generating the points using destPoint
    destPoint(center[1,],                    # Centering
              b=((i-1)*90):(90*i),           # 360 degrees (a complete circle)
              d=r_cardinal[1,i] * 1852) %>%  # radius
      
      rbind(center) %>% # Adding center/origins
      
      as_tibble() -> df_example[[i]]
  }
  
  # Binds all data frames (ne, se, sw, and nw)
  bind_rows(df_example[[1]],
            df_example[[2]],
            df_example[[3]],
            df_example[[4]]) %>%
    
    ggplot() + # Ploting in ggplot2
    
    geom_polygon(data = df_example[[1]], # First sector NE
                 aes(x = lon, y = lat),
                 fill = data$wind_speed[1],
                 color = data$wind_speed[1],
                 alpha = 0.5) +
    
    geom_polygon(data = df_example[[2]], # Second sector SE
                 aes(x = lon, y = lat),
                 fill = data$wind_speed[1],
                 color = data$wind_speed[1],
                 alpha = 0.5) +
    
    geom_polygon(data = df_example[[3]], # Third sector SW
                 aes(x = lon, y = lat),
                 fill = data$wind_speed[1],
                 color = data$wind_speed[1],
                 alpha = 0.5) +
    
    geom_polygon(data = df_example[[4]], # Fourth sector NW
                 aes(x = lon, y = lat),
                 fill = data$wind_speed[1],
                 color = data$wind_speed[1],
                 alpha = 0.5)
}

# Importing dataset
data_manipulation(data_import()) %>%
  
  filter(storm_id %in% "KATRINA-2005",                                # Filtering an example
         date %in% lubridate::ymd_hm("2005-08-29-12-00")) -> katrina  # KATRINA 2005

# Using the function
geom_beta(data = katrina)

# Function geom_beta2
geom_beta2 <- function(data = data,
                       x = longitude,
                       y = latitude,
                       r_ne = ne,
                       r_se = se,
                       r_nw = nw,
                       r_sw = sw,
                       fill = wind_speed,
                       color = wind_speed)
{
  # Creating a list
  df_example <- as_tibble()
  
  # Center of the hurricane
  center <- cbind(data$longitude,
                  data$latitude)
  
  # Storing the speed in data frame
  r_cardinal <- cbind(data$ne, # Atention to the sequence of the sector!
                      data$se, # Must be in this order to be correct.
                      data$sw, # Clockwise starting in 12 o'clock as zero degrees
                      data$nw) #
  
  # My loop to create the for quadrants
  for (i in 1:4)
  {
    # Loop to create the 34, 50 and 64 knot areas
    for (j in 1:nrow(data))
    {
      # Generating the points
      destPoint(center[j,],
                b=((i-1)*90):(90*i),           # 360 degrees (a complete circle)
                d=r_cardinal[j,i] * 1852) %>%  # radius
        
        rbind(center) %>% # Adding center/origins
        
        as_tibble() %>% # Converting regular data frame to tibble
        
        mutate(i = i,      # Adding columns i and j
               j = j) %>%  # Later I will use to filter
        
        bind_rows(df_example) -> df_example
    }
  }
  
  # Ploting with ggplot2
  ggplot() + 
    
    geom_polygon(data = (df_example %>%    # Sector NE
                           filter(i %in% 1,   # 64 knot
                                  j %in% 1)),
                 aes(x = lon, y = lat,
                     fill = data$wind_speed[1]),
                 alpha = 0.5) +
    
    geom_polygon(data = (df_example %>%    # Sector SE
                           filter(i %in% 2,   # 64 knot
                                  j %in% 1)),
                 aes(x = lon, y = lat,
                     fill = data$wind_speed[1]),
                 alpha = 0.5) + 
    
    geom_polygon(data = (df_example %>%    # Sector SW
                           filter(i %in% 3,   # 64 knot
                                  j %in% 1)),
                 aes(x = lon, y = lat,
                     fill = data$wind_speed[1]),
                 alpha = 0.5) +
    
    geom_polygon(data = (df_example %>%    # Sector NW
                           filter(i %in% 4,   # 64 knot
                                  j %in% 1)),
                 aes(x = lon, y = lat,
                     fill = data$wind_speed[1]),
                 alpha = 0.5)+ 
    
    geom_polygon(data = (df_example %>%    # Sector NE
                           filter(i %in% 1,   # 50 knot
                                  j %in% 2)),
                 aes(x = lon, y = lat,
                     fill = data$wind_speed[2]),
                 alpha = 0.5) + 
    
    geom_polygon(data = (df_example %>%    # Sector SE
                           filter(i %in% 2,   # 50 knot
                                  j %in% 2)),
                 aes(x = lon, y = lat,
                     fill = data$wind_speed[2]),
                 alpha = 0.5) + 
    
    geom_polygon(data = (df_example %>%    # Sector SW
                           filter(i %in% 3,   # 50 knot
                                  j %in% 2)),
                 aes(x = lon, y = lat,
                     fill = data$wind_speed[2]),
                 alpha = 0.5) + 
    
    geom_polygon(data = (df_example %>%    # Sector NW
                           filter(i %in% 4,   # 50 knot
                                  j %in% 2)),
                 aes(x = lon, y = lat,
                     fill = data$wind_speed[2]),
                 alpha = 0.5) + 
    
    geom_polygon(data = (df_example %>%    # Sector NE
                           filter(i %in% 1,   # 34 knot
                                  j %in% 3)),
                 aes(x = lon, y = lat,
                     fill = data$wind_speed[3]),
                 alpha = 0.5) + 
    
    geom_polygon(data = (df_example %>%    # Sector SE
                           filter(i %in% 2,   # 34 knot
                                  j %in% 3)),
                 aes(x = lon, y = lat,
                     fill = data$wind_speed[3]),
                 alpha = 0.5) + 
    
    geom_polygon(data = (df_example %>%    # Sector SW
                           filter(i %in% 3,   # 34 knot
                                  j %in% 3)),
                 aes(x = lon, y = lat,
                     fill = data$wind_speed[3]),
                 alpha = 0.5) + 
    
    geom_polygon(data = (df_example %>%    # Sector NW
                           filter(i %in% 4,   # 34 knot
                                  j %in% 3)),
                 aes(x = lon, y = lat,
                     fill = data$wind_speed[3]),
                 alpha = 0.5)
}

# Using the function
geom_beta2(data = katrina)

# Importing dataset
data_manipulation(data_import()) %>%
  
  filter(storm_id %in% "KATRINA-2005",                                # Filtering an example
         date %in% lubridate::ymd_hm("2005-08-29-12-00")) -> katrina  # KATRINA 2005

# Using the function
geom_beta2(data = katrina) +
  
  scale_color_manual(name = "Wind speed (kts)",
                     values = c("red",
                                "orange",
                                "yellow")) +
  
  scale_fill_manual(name = "Wind speed (kts)",
                    values = c("red",
                               "orange",
                               "yellow"))

# Function geom_beta2
geom_beta_map <- function(data = data,
                          x = longitude,
                          y = latitude,
                          r_ne = ne,
                          r_se = se,
                          r_nw = nw,
                          r_sw = sw,
                          fill = wind_speed,
                          color = wind_speed)
{
  # Creating a list
  df_example <- as_tibble()
  
  # Center of the hurricane
  center <- cbind(data$longitude,
                  data$latitude)
  
  # Storing the speed in data frame
  r_cardinal <- cbind(data$ne, # Atention to the sequence of the sector!
                      data$se, # Must be in this order to be correct.
                      data$sw, # Clockwise starting in 12 o'clock as zero degrees
                      data$nw) #
  
  # My loop to create the for quadrants
  for (i in 1:4)
  {
    # Loop to create the 34, 50 and 64 knot areas
    for (j in 1:nrow(data))
    {
      # Generating the points
      destPoint(center[j,],
                b=((i-1)*90):(90*i),           # 360 degrees (a complete circle)
                d=r_cardinal[j,i] * 1852) %>%  # radius
        
        rbind(center) %>% # Adding center/origins
        
        as_tibble() %>% # Converting regular data frame to tibble
        
        mutate(i = i,      # Adding columns i and j
               j = j) %>%  # Later I will use to filter
        
        bind_rows(df_example) -> df_example
    }
  }
  
  # Loading ggmap package
  library(ggmap)
  
  # API Key
  register_google(key = "AIzaSyB0fKSElDN-a0LpvhvvWlFNP5CWCFf3jZM")
  
  # Google Maps/Stratmen
  get_map("Louisiana",
          zoom = 6,
          maptype = "toner-background") %>%
    
    # Ploting with ggmap
    ggmap(extent = "device") + 
    
    geom_polygon(data = (df_example %>%    # Sector NE
                           filter(i %in% 1,   # 64 knot
                                  j %in% 1)),
                 aes(x = lon, y = lat,
                     fill = data$wind_speed[1]),
                 alpha = 0.5) +
    
    geom_polygon(data = (df_example %>%    # Sector SE
                           filter(i %in% 2,   # 64 knot
                                  j %in% 1)),
                 aes(x = lon, y = lat,
                     fill = data$wind_speed[1]),
                 alpha = 0.5) + 
    
    geom_polygon(data = (df_example %>%    # Sector SW
                           filter(i %in% 3,   # 64 knot
                                  j %in% 1)),
                 aes(x = lon, y = lat,
                     fill = data$wind_speed[1]),
                 alpha = 0.5) +
    
    geom_polygon(data = (df_example %>%    # Sector NW
                           filter(i %in% 4,   # 64 knot
                                  j %in% 1)),
                 aes(x = lon, y = lat,
                     fill = data$wind_speed[1]),
                 alpha = 0.5)+ 
    
    geom_polygon(data = (df_example %>%    # Sector NE
                           filter(i %in% 1,   # 50 knot
                                  j %in% 2)),
                 aes(x = lon, y = lat,
                     fill = data$wind_speed[2]),
                 alpha = 0.5) + 
    
    geom_polygon(data = (df_example %>%    # Sector SE
                           filter(i %in% 2,   # 50 knot
                                  j %in% 2)),
                 aes(x = lon, y = lat,
                     fill = data$wind_speed[2]),
                 alpha = 0.5) + 
    
    geom_polygon(data = (df_example %>%    # Sector SW
                           filter(i %in% 3,   # 50 knot
                                  j %in% 2)),
                 aes(x = lon, y = lat,
                     fill = data$wind_speed[2]),
                 alpha = 0.5) + 
    
    geom_polygon(data = (df_example %>%    # Sector NW
                           filter(i %in% 4,   # 50 knot
                                  j %in% 2)),
                 aes(x = lon, y = lat,
                     fill = data$wind_speed[2]),
                 alpha = 0.5) + 
    
    geom_polygon(data = (df_example %>%    # Sector NE
                           filter(i %in% 1,   # 34 knot
                                  j %in% 3)),
                 aes(x = lon, y = lat,
                     fill = data$wind_speed[3]),
                 alpha = 0.5) + 
    
    geom_polygon(data = (df_example %>%    # Sector SE
                           filter(i %in% 2,   # 34 knot
                                  j %in% 3)),
                 aes(x = lon, y = lat,
                     fill = data$wind_speed[3]),
                 alpha = 0.5) + 
    
    geom_polygon(data = (df_example %>%    # Sector SW
                           filter(i %in% 3,   # 34 knot
                                  j %in% 3)),
                 aes(x = lon, y = lat,
                     fill = data$wind_speed[3]),
                 alpha = 0.5) + 
    
    geom_polygon(data = (df_example %>%    # Sector NW
                           filter(i %in% 4,   # 34 knot
                                  j %in% 3)),
                 aes(x = lon, y = lat,
                     fill = data$wind_speed[3]),
                 alpha = 0.5)
}

# Using the function
geom_beta_map(data = katrina) +
  
  scale_color_manual(name = "Wind speed (kts)",
                     values = c("red",
                                "orange",
                                "yellow")) +
  
  scale_fill_manual(name = "Wind speed (kts)",
                    values = c("red",
                               "orange",
                               "yellow"))

# Function geom_beta2
geom_beta_map_scale <- function(data = data,
                                x = longitude,
                                y = latitude,
                                r_ne = ne,
                                r_se = se,
                                r_nw = nw,
                                r_sw = sw,
                                fill = wind_speed,
                                color = wind_speed,
                                scale_radii = 1)
{
  # Creating a data frame
  df_example <- as_tibble()
  
  # Center of the hurricane
  center <- cbind(data$longitude,
                  data$latitude)
  
  # Storing the speed in data frame
  r_cardinal <- cbind(data$ne * scale_radii, # Atention to the sequence of the sector!
                      data$se * scale_radii, # Must be in this order to be correct.
                      data$sw * scale_radii, # Clockwise starting in 12 o'clock as zero degrees
                      data$nw * scale_radii) #
  
  # My loop to create the for quadrants
  for (i in 1:4)
  {
    # Loop to create the 34, 50 and 64 knot areas
    for (j in 1:nrow(data))
    {
      # Generating the points
      destPoint(center[j,],
                b=((i-1)*90):(90*i),           # 360 degrees (a complete circle)
                d=r_cardinal[j,i] * 1852) %>%  # radius
        
        rbind(center) %>% # Adding center/origins
        
        as_tibble() %>% # Converting regular data frame to tibble
        
        mutate(i = i,      # Adding columns i and j
               j = j) %>%  # Later I will use to filter
        
        bind_rows(df_example) -> df_example
    }
  }
  
  # Loading ggmap package
  library(ggmap)
  
  # API Key
  register_google(key = "AIzaSyB0fKSElDN-a0LpvhvvWlFNP5CWCFf3jZM")
  
  # Google Maps/Stratmen
  get_map("Louisiana",
          zoom = 6,
          maptype = "toner-background") %>%
    
    # Ploting with ggmap
    ggmap(extent = "device") + 
    
    geom_polygon(data = (df_example %>%    # Sector NE
                           filter(i %in% 1,   # 64 knot
                                  j %in% 1)),
                 aes(x = lon, y = lat,
                     fill = data$wind_speed[1]),
                 alpha = 0.5) +
    
    geom_polygon(data = (df_example %>%    # Sector SE
                           filter(i %in% 2,   # 64 knot
                                  j %in% 1)),
                 aes(x = lon, y = lat,
                     fill = data$wind_speed[1]),
                 alpha = 0.5) + 
    
    geom_polygon(data = (df_example %>%    # Sector SW
                           filter(i %in% 3,   # 64 knot
                                  j %in% 1)),
                 aes(x = lon, y = lat,
                     fill = data$wind_speed[1]),
                 alpha = 0.5) +
    
    geom_polygon(data = (df_example %>%    # Sector NW
                           filter(i %in% 4,   # 64 knot
                                  j %in% 1)),
                 aes(x = lon, y = lat,
                     fill = data$wind_speed[1]),
                 alpha = 0.5)+ 
    
    geom_polygon(data = (df_example %>%    # Sector NE
                           filter(i %in% 1,   # 50 knot
                                  j %in% 2)),
                 aes(x = lon, y = lat,
                     fill = data$wind_speed[2]),
                 alpha = 0.5) + 
    
    geom_polygon(data = (df_example %>%    # Sector SE
                           filter(i %in% 2,   # 50 knot
                                  j %in% 2)),
                 aes(x = lon, y = lat,
                     fill = data$wind_speed[2]),
                 alpha = 0.5) + 
    
    geom_polygon(data = (df_example %>%    # Sector SW
                           filter(i %in% 3,   # 50 knot
                                  j %in% 2)),
                 aes(x = lon, y = lat,
                     fill = data$wind_speed[2]),
                 alpha = 0.5) + 
    
    geom_polygon(data = (df_example %>%    # Sector NW
                           filter(i %in% 4,   # 50 knot
                                  j %in% 2)),
                 aes(x = lon, y = lat,
                     fill = data$wind_speed[2]),
                 alpha = 0.5) + 
    
    geom_polygon(data = (df_example %>%    # Sector NE
                           filter(i %in% 1,   # 34 knot
                                  j %in% 3)),
                 aes(x = lon, y = lat,
                     fill = data$wind_speed[3]),
                 alpha = 0.5) + 
    
    geom_polygon(data = (df_example %>%    # Sector SE
                           filter(i %in% 2,   # 34 knot
                                  j %in% 3)),
                 aes(x = lon, y = lat,
                     fill = data$wind_speed[3]),
                 alpha = 0.5) + 
    
    geom_polygon(data = (df_example %>%    # Sector SW
                           filter(i %in% 3,   # 34 knot
                                  j %in% 3)),
                 aes(x = lon, y = lat,
                     fill = data$wind_speed[3]),
                 alpha = 0.5) + 
    
    geom_polygon(data = (df_example %>%    # Sector NW
                           filter(i %in% 4,   # 34 knot
                                  j %in% 3)),
                 aes(x = lon, y = lat,
                     fill = data$wind_speed[3]),
                 alpha = 0.5)
}

# Using the function
geom_beta_map_scale(data = katrina,scale_radii = 0.5) +
  
  scale_color_manual(name = "Wind speed (kts)",
                     values = c("red",
                                "orange",
                                "yellow")) +
  
  scale_fill_manual(name = "Wind speed (kts)",
                    values = c("red",
                               "orange",
                               "yellow")) +
  
  ggtitle(label = "scale_radii = 0.5") -> gp_half

# Using the function
geom_beta_map_scale(data = katrina,scale_radii = 1.0) +
  
  scale_color_manual(name = "Wind speed (kts)",
                     values = c("red",
                                "orange",
                                "yellow")) +
  
  scale_fill_manual(name = "Wind speed (kts)",
                    values = c("red",
                               "orange",
                               "yellow")) +
  
  ggtitle(label = "scale_radii = 1.0") -> gp_plain

# Loading package
library(gridExtra)

# Ploting in a grid style
grid.arrange(gp_plain, # First column scale_radii  = 1.0
             gp_half,  # Second column scale_radii = 0.5
             ncol = 2)

# Requeriments 
required_aes = c("x",   # x = longitude
                 "y",     # y = latitude
                 "r_ne",  # Northeast radius
                 "r_se",  # Southeast radius
                 "r_sw",  # Southwest radius
                 "r_nw")  # Northwest readius

default_aes = aes(colour      = "black", # Line color
                  fill        = "black", # Standard Fill color
                  linetype    = 0,       # No line
                  alpha       = 0.65,    # Transparency
                  scale_radii = 1.0)     # Default value (no reduction)

# Defining the draw_key
draw_key = draw_key_polygon

# Building the draw_group
draw_group = function(data,         # a data frame with one column for each aesthetic.
                      panel_params,  # don't look inside it, just pass along to coord methods.
                      coord) {       # coordinate system.
  
  # Creating a data frame
  df_example <- as_tibble()
  
  # Center of the hurricane
  center <- cbind(data$longitude,
                  data$latitude)
  
  # Storing the speed in data frame
  r_cardinal <- cbind(data$ne * scale_radii, # Atention to the sequence of the sector!
                      data$se * scale_radii, # Must be in this order to be correct.
                      data$sw * scale_radii, # Clockwise starting in 12 o'clock as zero degrees
                      data$nw * scale_radii) #
  
  # My loop to create the for quadrants
  for (i in 1:4)
  {
    # Loop to create the 34, 50 and 64 knot areas
    for (j in 1:nrow(data))
    {
      # Generating the points
      destPoint(center[j,],
                b=((i-1)*90):(90*i),           # 360 degrees (a complete circle)
                d=r_cardinal[j,i] * 1852) %>%  # radius covertion to meters
        
        rbind(center) %>% # Adding center/origins
        
        as_tibble() %>% # Converting regular data frame to tibble
        
        mutate(i = i,      # Adding columns i and j
               j = j) %>%  # Later I will use to filter
        
        bind_rows(df_example) -> df_example
    }
  }
  
  grid::polygonGrob(x = coords$x,
                    y = coords$y,
                    gp = grid::gpar(col = color,
                                    fill = fill,
                                    alpha = alpha))
}

# Defining the Geom class
GeomHurricane <- ggplot2::ggproto("GeomHurricane",
                                  Geom,
                                  required_aes = c("x",  # x = longitude
                                                   "y",     # y = latitude
                                                   "r_ne",  # Northeast radius
                                                   "r_se",  # Southeast radius
                                                   "r_sw",  # Southwest radius
                                                   "r_nw"), # Northwest radius
                                  
                                  default_aes = ggplot2::aes(colour  = "black",  # Line color
                                                             fill        = "black",  # Standard Fill color
                                                             linetype    = 0,        # No line
                                                             alpha       = 0.65,     # Transparency
                                                             scale_radii = 1.0),     # Default value (no reduction)
                                  
                                  draw_key = draw_key_polygon,
                                  
                                  draw_group = function(data,
                                                        panel_scales,
                                                        coord) {
                                    # Creating a data frame
                                    df_hurricane <- dplyr::as_tibble()
                                    center       <- dplyr::as_tibble()
                                    
                                    # Adding new columns to de data
                                    data %>% dplyr::mutate(fill = fill,     # Creating columns to assign variables
                                                           colour = colour) #
                                    
                                    # Center of the hurricane
                                    data %>% dplyr::select(lon = x,           # longitude
                                                           lat = y) -> center # latitude
                                    
                                    # Calculating the area/radius
                                    data %>% dplyr::select(r_ne,       # 
                                                           r_se,       # Subsetting
                                                           r_sw,       #
                                                           r_nw) %>%   #
                                      
                                      dplyr::mutate(r_ne = data$scale_radii * r_ne * 1852, # Converting nautical knots 
                                                    r_se = data$scale_radii * r_se * 1852, # to meters : knots * 1852
                                                    r_sw = data$scale_radii * r_sw * 1852, # scale_radii : scale variable
                                                    r_nw = data$scale_radii * r_nw * 1852) -> radius
                                    
                                    # Loop to create the for quadrants (columns)
                                    for (i in 1:4)
                                    {
                                      # For each quadrant: Loop to create the 34, 50 and 64 knot areas (rows)
                                      for (j in 1:nrow(data))
                                      {
                                        # Generating the points
                                        geosphere::destPoint(c(x = center[j,1],        # Center of the "circle"
                                                               y = center[j,2]),       # 
                                                             b = ((i-1)*90):(90*i),  # 360 degrees (a complete circle)
                                                             d = radius[j,i]) %>%    # radius
                                          
                                          rbind(c(x = center[j,1],       # Longitude
                                                  y = center[j,2])) %>%  # Latitude
                                          
                                          rbind(df_hurricane) -> df_hurricane # Output: Will be stacked over iteration
                                      }
                                      
                                      # Data Manipulation
                                      df_hurricane %>% 
                                        
                                        dplyr::as_tibble() %>% # Converting to tibble
                                        
                                        dplyr::rename(x = lon,      # Renaming columns
                                                      y = lat) %>%  # The ouput of destPoint() function has lon and lat as names.
                                        
                                        coord$transform(panel_scales) -> quadrant_points # Cleaned data redy to plot
                                    }
                                    
                                    # Plot the polygon
                                    grid::polygonGrob(x = quadrant_points$x,   # Longitude
                                                      y = quadrant_points$y,   # Latitude
                                                      default.units = "native",
                                                      gp = grid::gpar(col = data$colour,  # Using line color given
                                                                      fill = data$fill,   # Using fill color given
                                                                      alpha = data$alpha, # Default value
                                                                      lty = 1,            # Default value
                                                                      scale_radii = data$scale_radii))   # scale_radii       
                                  }
)

# Default functions
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

# Loading ggmap package
library(ggmap)

# API Key
register_google(key = "AIzaSyB0fKSElDN-a0LpvhvvWlFNP5CWCFf3jZM")

# Google Maps/Stratmen
get_map("Louisiana",
        zoom = 6,
        maptype = "toner-background") %>%
  
  # Ploting with ggmap
  ggmap(extent = "device") + 
  
  geom_hurricane(data= katrina,
                 aes(x = longitude,
                     y = latitude,
                     r_ne = ne,
                     r_se = se,
                     r_nw = nw,
                     r_sw = sw,
                     fill = wind_speed,
                     color = wind_speed)) +
  
  scale_color_manual(name = "Wind speed (kts)",
                     values = c("red",
                                "orange",
                                "yellow")) +
  
  scale_fill_manual(name = "Wind speed (kts)",
                    values = c("red",
                               "orange",
                               "yellow"))

# Importing dataset
data_manipulation(data_import()) %>%
  
  filter(storm_id %in% "IKE-2008",
         date %in% lubridate::ymd_hm("2008-09-11 18:00")) -> ike_2008 # Data set with 2008 Ike values

# Loading ggmap package
library(ggmap)

# API Key
register_google(key = "AIzaSyB0fKSElDN-a0LpvhvvWlFNP5CWCFf3jZM")

# Google Maps/Stratmen
get_map(location = c(-88.9,25.8), # c(longitude,latitude)
        zoom = 5,
        maptype = "toner-background") %>%
  
  # Saving the map
  ggmap(extent = "device") -> base_map

# Ploting with ggmap
base_map + geom_hurricane(data= ike_2008,
                          aes(x = longitude,
                              y = latitude,
                              r_ne = ne,
                              r_se = se,
                              r_nw = nw,
                              r_sw = sw,
                              fill = wind_speed,
                              color = wind_speed)) +
  
  scale_color_manual(name = "Wind speed (kts)",
                     values = c("red",
                                "orange",
                                "yellow")) +
  
  scale_fill_manual(name = "Wind speed (kts)",
                    values = c("red",
                               "orange",
                               "yellow"))

# scale_radii = 1.0
base_map + geom_hurricane(data= ike_2008,
                          aes(x = longitude,
                              y = latitude,
                              r_ne = ne,
                              r_se = se,
                              r_nw = nw,
                              r_sw = sw,
                              fill = wind_speed,
                              color = wind_speed)) +
  
  scale_color_manual(name = "Wind speed (kts)",
                     values = c("red",
                                "orange",
                                "yellow")) +
  
  scale_fill_manual(name = "Wind speed (kts)",
                    values = c("red",
                               "orange",
                               "yellow")) -> example_1

# scale_radii = 0.5
base_map + geom_hurricane(data= ike_2008,
                          aes(x = longitude,
                              y = latitude,
                              r_ne = ne,
                              r_se = se,
                              r_nw = nw,
                              r_sw = sw,
                              fill = wind_speed,
                              color = wind_speed,
                              scale_radii = 0.5)) + 
  
  scale_color_manual(name = "Wind speed (kts)",
                     values = c("red",
                                "orange",
                                "yellow")) +
  
  scale_fill_manual(name = "Wind speed (kts)",
                    values = c("red",
                               "orange",
                               "yellow")) -> example_2

# Loading package
library(gridExtra)

# Ploting in a grid style
grid.arrange(example_1,  # First column scale_radii  = 1.0
             example_2,  # Second column scale_radii = 0.5
             ncol = 2)

# Loading the png package
library(png)

# Creating the png file
png(filename = "assignment_upload.png") # File name

base_map + geom_hurricane(data= ike_2008,
                          aes(x = longitude,
                              y = latitude,
                              r_ne = ne,
                              r_se = se,
                              r_nw = nw,
                              r_sw = sw,
                              fill = wind_speed,
                              color = wind_speed)) +
  
  scale_color_manual(name = "Wind speed (kts)",
                     values = c("red",
                                "orange",
                                "yellow")) +
  
  scale_fill_manual(name = "Wind speed (kts)",
                    values = c("red",
                               "orange",
                               "yellow")) + 
  
  labs(title = "2008-09-11 18:00 UTC - Ike Hurricane",
       subtitle = "AH Uyekita")

dev.off() # Closing device






