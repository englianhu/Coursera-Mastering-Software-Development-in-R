library(utils)
library(tidyverse)
library(tidyr)
library(lubridate)
library(dplyr)
library(ggmap)
library(grid)
library(mapdata)
library(geosphere)
library(tibble)

# read in data
ext_tracks_widths = c(7, 10, 2, 2, 3, 5, 5, 6, 4, 5, 4, 4, 5, 3, 4, 3, 3, 3,
                       4, 3, 3, 3, 4, 3, 3, 3, 2, 6, 1)
ext_tracks_colnames = c("storm_id", "storm_name", "month", "day",
                         "hour", "year", "latitude", "longitude",
                         "max_wind", "min_pressure", "rad_max_wind",
                         "eye_diameter", "pressure_1", "pressure_2",
                         paste("radius_34", c("ne", "se", "sw", "nw"), sep = "_"),
                         paste("radius_50", c("ne", "se", "sw", "nw"), sep = "_"),
                         paste("radius_64", c("ne", "se", "sw", "nw"), sep = "_"),
                         "storm_type", "distance_to_land", "final")

ext_tracks = read_fwf("ebtrk_atlc_1988_2015.txt", 
                       fwf_widths(ext_tracks_widths, ext_tracks_colnames),
                       na = "-99")


# format the table and get the data we want
dta = ext_tracks %>% 
    dplyr::mutate(storm_id= paste(storm_name, year, sep ="-")) %>%
    tidyr::unite(datetime, year, month, day, hour, sep="-") %>%
    dplyr::mutate(date= ymd_h(datetime))

dta = dta %>% tidyr::pivot_longer(cols = starts_with('radius'), 
    names_prefix = 'radius_', names_to = c('wind_speed', '.value'), 
    names_sep = '_')

dta$longitude = ifelse(is.na(dta$longitude), 0, dta$longitude)

dta$longitude = -dta$longitude

dta_h = dta %>% dplyr::filter(storm_id=='ROXANNE-1995' & datetime=='1995-10-11-06')

# API
api = "AIzaSyBxL0SF4VmKNnI9SSWfcPSk_yGuGm4_SlE"
register_google(key = api)

# make the geom
cir = ggproto("cir", Stat, 
    compute_group = function(data, scales) {

        out = tidyr::gather(data, key = "key", value=distance, 
          ne,se, nw, sw)
        
        out = out %>% dplyr::mutate(s = 0)
        out = out %>% dplyr::mutate(f = 0)
     
        out = out %>% dplyr::mutate(s = ifelse(key == "ne", 0, s ))
        out = out %>% dplyr::mutate(f = ifelse(key == "ne", 90, f ))

        out = out %>% dplyr::mutate(s = ifelse(key == "nw", 270, s ))
        out = out %>% dplyr::mutate(f = ifelse(key == "nw", 360, f ))                                                        

        out = out %>% dplyr::mutate(s = ifelse(key == "sw", 180, s ))
        out = out %>% dplyr::mutate(f = ifelse(key == "sw", 270, f )) 
        
        out = out %>% dplyr::mutate(s = ifelse(key == "se", 90, s ))
        out = out %>% dplyr::mutate(f = ifelse(key == "se", 180, f ))                               

        out = out %>% dplyr::mutate(speed = fill)
        out
    },
    required_aes = c("x", "y", "ne", "se", "nw", "sw")
)

map_cir = function(mapping = NULL, data = NULL, geom = "polygon",
    position = "identity", show.legend = NA, 
    outliers = TRUE, inherit.aes = TRUE, ...) {
    ggplot2::layer(
        stat = cir, 
        data = data, 
        mapping = mapping, 
        geom = geom, 
        position = position, 
        show.legend = show.legend, 
        inherit.aes = inherit.aes,
        params = list(outliers = outliers, ...))        
}

geom_h = ggproto("geom_h", Geom,
    required_aes = c("x", "y", "speed", "distance", "s", "f"),
    default_aes = aes(shape = 1, colour="red", fill = "red", 
        linetype =0, alpha =.70, scale_rad = 1),
    draw_key = draw_key_polygon,
    draw_group  = function(data, panel_scales, coord) {

    dta_hur = data.frame()
    data = data %>% dplyr::mutate(distance = distance * scale_rad * 1852)
    sp = data %>% dplyr::arrange(desc(speed))

    for (i in 1:nrow(sp)) {
        cx = sp[i, "x"]
        cy = sp[i, "y"]
        st = sp[i, "s"]
        ed = sp[i, "f"]
        distance = sp[i, "distance"]

        geosphere::destPoint(c(x = cx, y = cy), b = ((st):(ed)), d = distance) %>%
        rbind(c(x = cx, y = cy)) %>% 
        rbind(dta_hur) -> dta_hur
    }

    dta_hur %>% 
    dplyr::as_tibble() %>% # 
    dplyr::rename(x = lon,      
        y = lat) %>%  
    coord$transform(panel_scales) -> out_t

    grid::polygonGrob(x = out_t$x,
        y = out_t$y,
        default.units = "native",
        gp = grid::gpar(col = data$colour,  
            fill = data$fill,   
            alpha = data$alpha,
            lty = 1,            
            scale_rad = data$scale_rad)
    )
})

map_hur = function(mapping = NULL, data = NULL, stat = cir,
    position = "identity", na.rm = FALSE, 
    show.legend = NA, inherit.aes = TRUE, ...) {
    ggplot2::layer(
      geom = geom_h, mapping = mapping,  
      data = data, stat = stat, position = position, 
      show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, ...)
    )
}

# plot
get_map(location = c(-90,20), zoom = 6, maptype = "toner-background") %>%
    ggmap(extent = "device")  +
    map_hur(data = dta_h, aes(x = longitude, y = latitude, ne = ne, 
        se = se, nw = nw, sw = sw, fill = wind_speed, color = wind_speed, 
        scale_rad = 1)) +
    ggplot2::scale_color_manual(name = "Wind speed (kts)", values = c("red", "orange", "yellow"))+
    ggplot2::scale_fill_manual(name = "Wind speed (kts)", values = c("red", "orange", "yellow"))
 
