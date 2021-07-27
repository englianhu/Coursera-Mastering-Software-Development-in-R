
library(readr)
library(dplyr)
library(lubridate)
library(trelliscopejs)
library(geosphere)

'Lectura'

ancho <- c(7, 10, 2, 2, 3, 5, 5, 6, 4, 5, 4, 4, 5, 3, 4, 3, 3, 3,
                       4, 3, 3, 3, 4, 3, 3, 3, 2, 6, 1)
columnas <- c("storm_id", "storm_name", "month", "day",
                         "hour", "year", "latitude", "longitude",
                         "max_wind", "min_pressure", "rad_max_wind",
                         "eye_diameter", "pressure_1", "pressure_2",
                         paste("radius_34", c("ne", "se", "sw", "nw"), sep = "_"),
                         paste("radius_50", c("ne", "se", "sw", "nw"), sep = "_"),
                         paste("radius_64", c("ne", "se", "sw", "nw"), sep = "_"),
                         "storm_type", "distance_to_land", "final")

datos <- read_fwf("C:/hurricanes_data/ebtrk_atlc_1988_2015.txt", 
                   fwf_widths(ancho, columnas),na = "-99")

'Limpieza datos'

datos1 <- datos %>%
  select(storm_id, storm_name, month, day, hour, year, latitude, longitude, starts_with("radius_")
  ) %>%
  rename(original_id = storm_id) %>%
  mutate(storm_id = paste(storm_name, year, sep = "-"),
         date = make_datetime(year = as.integer(year), 
                              month = as.integer(month), 
                              day = as.integer(day),
                              hour = as.integer(hour),
                              tz = "UTC"),
         longitude = -longitude)

datos_f <- datos1  %>%
  select(storm_id, date, latitude, longitude, starts_with("radius_"))

datos_f_34 <- datos_f %>%
  select(storm_id, date, latitude, longitude,starts_with("radius_34_")) %>%
  rename(ne = radius_34_ne, se = radius_34_se, nw = radius_34_nw, sw = radius_34_sw) %>%
  mutate(wind_speed = 34)

datos_f_50 <- datos_f %>%
  select(storm_id, date, latitude, longitude,starts_with("radius_50_")) %>%
  rename(ne = radius_50_ne, se = radius_50_se, nw = radius_50_nw, sw = radius_50_sw) %>%
  mutate(wind_speed = 50)

datos_f_64 <- datos_f %>%
  select(storm_id, date, latitude, longitude,starts_with("radius_64_")) %>%
  rename(ne = radius_64_ne, se = radius_64_se, nw = radius_64_nw, sw = radius_64_sw) %>%
  mutate(wind_speed = 64)

datos_f_largo <- rbind(datos_f_34, datos_f_50, datos_f_64) %>%
  arrange(storm_id, date, wind_speed)


geom_hurricane <- function(mapping = NULL, data = NULL, stat = "identity",
                           position = "identity", na.rm = FALSE,
                           show.legend = NA, inherit.aes = TRUE, ...){
  ggplot2::layer(
    geom = GeomHurricane, mapping = mapping,
    data = data, stat = stat, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm,...)
  )
}

GeomHurricane <- ggplot2::ggproto(
  "GeomHurricane", 
  ggplot2::Geom,
  required_aes = c("x", "y", "r_ne", "r_se", "r_sw", "r_nw"),
  default_aes = ggplot2::aes(colour = "NA", fill = "grey20", size = 0.5, linetype = 1, alpha = 0.8, scale_radii = 1),
  draw_key = ggplot2::draw_key_polygon,
  draw_group = function(data, panel_scales, coord){
    
    point_obs = c(data[1,]$x, data[1,]$y)
    color <- data[1,]$colour
    fill <- data[1,]$fill
    alpha <- data[1,]$alpha
    scale_radii = data[1,]$scale_radii
    
    points_polygon = geosphere::destPoint(p = point_obs, b=1:90, d = data[1,]$r_ne * 1852 * scale_radii)
    data_ne <- data.frame(x = c(points_polygon[,"lon"], point_obs[1]),
                          y = c(points_polygon[,"lat"], point_obs[2])
    )
    
    points_polygon = geosphere::destPoint(p = point_obs, b=90:180, d = data[1,]$r_se * 1852 * scale_radii)
    data_se <- data.frame(x = c(points_polygon[,"lon"], point_obs[1]),
                          y = c(points_polygon[,"lat"], point_obs[2])
    )
    
    points_polygon = geosphere::destPoint(p = point_obs, b=180:270, d = data[1,]$r_sw * 1852 * scale_radii)
    data_sw <- data.frame(x = c(points_polygon[,"lon"], point_obs[1]),
                          y = c(points_polygon[,"lat"], point_obs[2])
    )
    
    points_polygon = geosphere::destPoint(p = point_obs, b=270:360, d = data[1,]$r_nw * 1852 * scale_radii)
    data_nw <- data.frame(x = c(points_polygon[,"lon"], point_obs[1]),
                          y = c(points_polygon[,"lat"], point_obs[2])
    )
    
    data_all <- rbind(data_ne, data_se, data_nw, data_sw)
    coords <- coord$transform(data_all, panel_scales)
    
    grid::polygonGrob(x = coords$x,
                      y = coords$y,
                      gp = grid::gpar(col = color, fill = fill, alpha = alpha))
  }
)



datos_f_largo$wind_speed <- as.factor(datos_f_largo$wind_speed)

tormenta_ike <- datos_f_largo[datos_f_largo$storm_id == "IKE-2008" & 
                                          datos_f_largo$date == lubridate::ymd_hms("2008-09-11 18:00:00"),]

tormenta_ike_2 <- datos_f_largo[datos_f_largo$storm_id == "IKE-2008" & 
                                  datos_f_largo$date == lubridate::ymd_hms("2008-09-12 06:00:00"),]

png("tormenta_ike_2_observaciones.png")
map_plot <- ggmap::get_map("Lousiana", zoom = 5, maptype = "toner-background")

map_plot %>%
  ggmap::ggmap(extent = "device") +
  geom_hurricane(data = tormenta_ike,
                 ggplot2::aes(x = longitude, y = latitude, 
                              r_ne = ne, r_se = se, r_nw = nw, r_sw = sw,
                              color = wind_speed, fill = wind_speed),scale_radii = 0.9) +
  geom_hurricane(data = tormenta_ike_2,
                 ggplot2::aes(x = longitude, y = latitude, 
                              r_ne = ne, r_se = se, r_nw = nw, r_sw = sw,
                              color = wind_speed, fill = wind_speed),scale_radii = 0.9) +
  ggplot2::scale_color_manual(name = "Wind speed (kts)", 
                              values = c("red", "orange", "yellow")) + 
  ggplot2::scale_fill_manual(name = "Wind speed (kts)", 
                             values = c("red", "orange", "yellow"))
dev.off()

png("hurricane_ike_observaciones.png")
map_plot <- ggmap::get_map("Lousiana", zoom = 5, maptype = "toner-background") 
map_plot %>%
  ggmap::ggmap(extent = "device") +
  geom_hurricane(data = tormenta_ike,
                 ggplot2::aes(x = longitude, y = latitude, 
                              r_ne = ne, r_se = se, r_nw = nw, r_sw = sw,
                              color = wind_speed, fill = wind_speed)) +
  ggplot2::scale_color_manual(name = "Wind speed (kts)", 
                              values = c("red", "orange", "yellow")) + 
  ggplot2::scale_fill_manual(name = "Wind speed (kts)", 
                             values = c("red", "orange", "yellow"))
dev.off()
