Mapping and interactive plots
合計点数 10


1. 質問 1

Consider the following data frame of points along the US border:
  
```
library(ggplot2)
us_map <- map_data("usa")
head(us_map, 3)

##      long      lat group order region subregion
## -101.4078 29.74224     1     1   main      <NA>
## -101.3906 29.74224     1     2   main      <NA>
## -101.3620 29.65056     1     3   main      <NA>
```
If you wanted to use this data to map the outline of the United States without converting the data to a spatial object, how would you map aesthetics in the ggplot call, and which geom would you add? 1点

- For the aesthetics, I would map long to x, lat to y, and group to group. I would then add a line geom. 
-✔️ For the aesthetics, I would map long to x, lat to y, and group to group. I would then add a path geom. 
- For the aesthetics, I would map lat to x, long to y, and group to group. I would then add a line geom. 
- For the aesthetics, I would map lat to x and long to y. I would then add a polygon geom. 


2. 質問 2

If you have a SpatialPointsDataframe object that you would like to use for mapping, why might you want to convert it to a data frame object and how would you do it? 1点

- You may want to convert it because data frames can be more easily manipulated with tidyverse tools. 
  To convert it, you could use the data_frame function from the dplyr package. 

- You may want to convert it because data frames are easier to write out to shapefiles. 
  To convert it, you could use the as.data.frame function. 

- A data frame that includes geographical information can be mapped by just running the plot function.
  To convert it, you could use the readShapePoints function from the maptools package.

-✔️ You may want to convert it because some plotting and mapping methods, include ggplot2 and ggmap, will only input a data frame. 
  To convert it, you could use the fortify function from ggplot2. 


3. 質問 3

Which of the following are geographic tasks can you do using functions from the ggmap package? 1点

-@0.83 Compute map distances using Google Maps.
-@0.83 Plot a ggmap object using the ggmap function.
-@0.83 Geocode addresses or other locations by inputting a character string with the address or location and getting back latitude and longitude listings using the Google Maps API.
- Reproject projected spatial data.
-@0.83 Get a base map for a certain location from GoogleMaps to which you can later add points, polygons, and other shapes.
-@0.83 Calculate the area of spatial polygons (e.g., calculate the area of US states based on a SpatialPolygons object).


4. 質問 4

The following code uses ggmap to plot the walking route between the US White House and the US Capitol Building: 
  
```
library(ggmap)
inauguration_route <- route(from = "US Capitol Building",
                            to = "White House", 
                            structure = "route",
                            mode = "walking")
inaug_route_map <- get_map("Metro Center, Washington DC",
                           zoom = 14) %>% 
  ggmap(extent = "device") + 
  geom_path(data = inauguration_route,
            color = "darkred", size = 1.1)
```
Say you have the following data frame with the addresses of a few hotels:
```
library(dplyr)
dc_hotels <- data_frame(address = 
                          c("1401 Pennsylvania Ave NW, Washington DC", 
                            "1331 Pennsylvania Ave NW, Washington DC")) %>%
  bind_cols(geocode(dc_hotels$address))
dc_hotels

#                                  address       lon      lat
#                                    <chr>     <dbl>    <dbl>
#  1401 Pennsylvania Ave NW, Washington DC -77.03227 38.89660
```
How would you create a new map with these hotels added as points? 1点

-
```
inaug_route_map + 
  geom_point(aes(x = lon, y = lat))
```

-
```
inauguration_route + 
  geom_point(data = dc_hotels, aes(x = lon, y = lat))
```

-✔️
```
inaug_route_map + 
  geom_point(data = dc_hotels, aes(x = lon, y = lat))
```

-
```
inaug_route_map + 
  geom_point(dc_hotels, aes(x = lon, y = lat))
```


5. 質問 5

What is a shapefile? 1点

- A spatial object in R, with slots for an associated data frame as well as for other elements like a bounding box. 
- An object created using ggmap. 
- An interactive map created using leaflet. 
- An object created using the get_map function from the ggmap package. 
-✔️ A format for saving spatial data. The format is not specific to R, but shapefiles can be read into or written from R using functions from packages like rgdal.


6. 質問 6

Why might you get an error running the following code?
  
```
library(ggmap)
get_map("Washington, DC")
```
1点

-✔️ Your computer is offline. The get_map function using the Google Maps API to pull the requested map into R. The function cannot post the request or receive the requested map if your computer is not online. 
- You have not initialized a ggplot object. 
- You must use longitude and latitude to request a map from the Google Maps API using this function. 
- You have not initialized a ggmap object. 


7. 質問 7

TRUE or FALSE: To use data you have cleaned in R to create an interactive plot, you must export the data and code the interactive plot in JavaScript. 1点

- TRUE
-✔️ FALSE


8. 質問 8

In which of the following formats can you interact (zoom, pan, open popups) with a map created using the leaflet package? 1点

-✔️ If you include the leaflet output in an R Markdown document that is rendered to HTML.
- If you include the leaflet output in an R Markdown document that is rendered to Word.
- If you include the leaflet output in an R Markdown document that is rendered to pdf.
-✔️ If you run the leaflet code in RStudio. 
-✔️ If you render the leaflet object in a Shiny App. 


9. 質問 9

You have data with spatial locations (e.g., sampling sites for a study) that you want to use to create an interactive leaflet map. 

Which of the following statements is true? 1点

-@0.6 The data can be in either a data frame with columns for latitude and longitude or a SpatialPoints object. 
-@0.6 The data must be in a SpatialPoints object, not a data frame. 
-@ You must specify a bounding box for the leaflet map to start zoomed in on the points you've mapped when the map is first rendered.
- If the data is in a data frame, the columns for latitude and longitude must be named lat and lng, respectively. 
-@0.6 The data must be in a data frame with columns for latitude and longitude, not a spatial object. 


10. 質問 10

Say you use the following code to plot a choropleth of murder arrests per 100,000 people in US states in 1973:

```
library(ggplot2)
library(dplyr)
USArrests %>%
  mutate(region = tolower(rownames(USArrests))) %>%
  left_join(map_data("state"), by = "region") %>%
  ggplot(aes(x = long, y = lat, group = group, fill = Murder)) + 
  geom_polygon() + 
  theme_void()
```
Which of the following statements are true about this code? 1点

-@ The code is using piping (using %>% from the dplyr package) to clean up and join together geospatial data (state borders) with the USArrests data frame, and then piping this into ggplot2 functions to create the map.
- The data on state boundaries used to create this map must be in a spatial object.
- The theme is added to the ggplot object to prevent x- and y-axes, x- and y-axis labels, and the background grid from being included on the final map.
- The theme is added to the ggplot object to clear off previous points plotted on the object.
-@ The map would have looked exactly the same if the Murder variable were mapped to color rather than fill.
-@ The group aesthetic is mapped to the group column in the data to create separate polygons for each state and to prevent unwanted lines between the borders of different states.

