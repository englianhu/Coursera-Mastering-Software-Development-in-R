#' @title load_hurricane
#' @description
#' Load Hurricane Data
#'
#' @importFrom readr read_fwf fwf_widths fwf_positions
#' @importFrom lubridate ymd_h
#' @importFrom reshape2 melt
#' @importFrom stringr str_split_fixed
#' @importFrom dplyr select mutate
#' @param dataDirectory Where the Data is stored
#' @param fileName Name of the file
#' @examples
#' load_hurricane(workdir = "./010_data/hurricanes_data", fileName = "ebtrk_atlc_1988_2015.txt")
#'
#' @export
load_hurricane <- function(workdir, filename) {
  df <- readr::read_fwf(file = paste0(workdir, filename),
                 readr::fwf_positions(c(8, 18, 25, 29, 35, 67, 70, 73, 76, 80, 83, 86, 89, 93, 96, 99, 102),
                               c(17,24, 28, 34, 40, 69, 72, 75, 78, 82, 85, 88, 91, 95, 98, 101, 104),
                               c("Storm.Name", "Month.Day.Hour", "Year", "Lat", "Long",
                                 "R.34.NE", "R.34.SE", "R.34.SW", "R.34.NW",
                                 "R.50.NE", "R.50.SE", "R.50.SW", "R.50.NW",
                                 "R.64.NE", "R.64.SE", "R.64.SW", "R.64.NW"
                               )
                 )
  )

  df2 <- df %>%
    dplyr::mutate(storm_id = paste0(Storm.Name, "-", Year),
           Lat = as.numeric(Lat),
           Long = as.numeric(Long),
           Long = ifelse(Long < 180, -Long, 360-Long),
           date = ymd_h(paste0(Year, Month.Day.Hour)),
           R.34 = paste(R.34.NE, R.34.NW, R.34.SE, R.34.SW, sep = ';'),
           R.50 = paste(R.50.NE, R.50.NW, R.50.SE, R.50.SW, sep = ';'),
           R.64 = paste(R.64.NE, R.64.NW, R.64.SE, R.64.SW, sep = ';')
    ) %>%
    dplyr::select(storm_id, date, Lat, Long, R.34, R.50, R.64)

  df2 <- reshape2::melt(df2, id.vars = c("storm_id", "date", "Lat", "Long")) %>%
    dplyr::mutate(
      wind_speed = case_when(
        variable == "R.34" ~ 34,
        variable == "R.50" ~ 50,
        variable == "R.64" ~ 64),
      ne = as.numeric(stringr::str_split_fixed(value, ';', 4)[,1]),
      nw = as.numeric(stringr::str_split_fixed(value, ';', 4)[,2]),
      se = as.numeric(stringr::str_split_fixed(value, ';', 4)[,3]),
      sw = as.numeric(stringr::str_split_fixed(value, ';', 4)[,4])
    ) %>%
    dplyr::select(-variable, -value)

  return (df2)
}

#' @title select_tracks
#' @description
#' Select specific storm observation at a certain datetime
#'
#' @import dplyr
#' @import lubridate
#'
#' @param data
#' @param storm_id
#' @param dt date of the storm in yyyy:mm:dd
#' @param tm time of the storm in hh:mm:ss
select_tracks <- function(data = data, storm_id = "IKE-2008", dt = "2008-09-13", tm = "12:00:00") {
  df <- data %>%
    filter(storm_id == "IKE-2008" & date == lubridate::ymd_hms(paste(dt, tm)))
}

#' @title GeomHurricane
#' @description
#' This constructs the ggproto object necessary to display the hurricane radii
#' plots.
#' @import ggplot2
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr left_join
#' @importFrom dplyr group_by_
#' @importFrom dplyr summarise
#' @importFrom dplyr ungroup
#' @importFrom dplyr filter
#' @importFrom dplyr bind_rows
#' @importFrom dplyr bind_cols
#' @importFrom dplyr rename
#' @importFrom tidyr gather
#' @importFrom grid polygonGrob
#' @importFrom grid gpar
#' @importFrom geosphere destPoint
#'
#'
#' @export

GeomHurricane <- ggproto("GeomHurricane", Geom,
                         required_aes = c("x", "y", "r_ne", "r_nw", "r_se", "r_sw"),
                         default_aes = aes(alpha = 0.5, scale_radii = 1.0, fill = 1, colour = 1),
                         draw_key = draw_key_polygon,
                         draw_group = function(data, panel_scales, coord) {

                           ## convert to meters and scale by scale_radaii
                           data <- data %>%
                             dplyr::mutate(r_ne = r_ne * 1852 * scale_radii,
                                    r_se = r_se * 1852 * scale_radii,
                                    r_nw = r_nw * 1852 * scale_radii,
                                    r_sw = r_sw * 1852 * scale_radii,
                                    fill = fill,
                                    colour = colour
                             )

                           # View(data)
                           df<- by(data, INDICES = 1:nrow(data), FUN = function(item) {
                             seg_ne <- geosphere::destPoint(c(item$x, item$y), b = 0:90, d = item$r_ne) %>%
                               as.data.frame() %>% mutate(fill = item$fill, col = item$colour)
                             seg_nw <- geosphere::destPoint(c(item$x, item$y), b = -90:0, d = item$r_nw) %>%
                               as.data.frame() %>% mutate(fill = item$fill, col = item$colour)
                             seg_se <- geosphere::destPoint(c(item$x, item$y), b = 90:180, d = item$r_se) %>%
                               as.data.frame() %>% mutate(fill = item$fill, col = item$colour)
                             seg_sw <- geosphere::destPoint(c(item$x, item$y), b = -180:-90, d = item$r_sw) %>%
                               as.data.frame() %>% mutate(fill = item$fill, col = item$colour)
                             return (rbind(seg_ne, seg_se, seg_sw, seg_nw))
                           }) %>% dplyr::bind_rows() %>%
                             dplyr::rename(x = lon, y = lat)

                           # View(df)
                           coords <- coord$transform(df, panel_scales)

                           #View(coords)

                           ## Contruct a grid grob
                           grid::polygonGrob(
                             x = coords$x,
                             y = coords$y,
                             # gp = grid::gpar(lty = "solid", lwd = 2)
                             gp = grid::gpar(fill = coords$fill, col = coords$colour,
                                             lty = "solid", lwd = 2)
                           )

                         }
)

#' @title geom_hurricane
#' @description
#' Generates a radii plot on a ggmap object representing the maximum windspeed
#' @inheritParams ggplot2::geom_polygon
#' @param scale_radii Numeric value to adjust scale of the radii plot.
#' @examples
#' \dontrun{
#' get_map("Louisiana", zoom = 6, maptype = "toner-background", source="stamen",force=FALSE) %>%
#'     ggmap(extent = "device") +
#'     geom_hurricane(data = katrina_observation,
#'                    aes(x = longitude, y = latitude,
#'                        r_ne = ne, r_se = se, r_nw = nw, r_sw = sw,
#'                        fill = wind_speed, color = wind_speed,
#'                        scale_radii=0.75)) +
#'     scale_color_manual(name = "Wind speed (kts)",
#'                        values = c("red", "orange", "yellow")) +
#'     scale_fill_manual(name = "Wind speed (kts)",
#'                       values = c("red", "orange", "yellow"))
#' }
#' @import ggplot2
#'
#' @export
geom_hurricane <- function(mapping = NULL, data = NULL, stat = "identity",
                           position = "identity", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomHurricane, mapping =  mapping,
    data = data, stat = stat, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )

}

library(dplyr)
library(lubridate)
library(ggplot2)

workdir <- "D:/Dropbox/05. Continuing education/53b. Data Visualisation in R/"
data_file <- "ebtrk_atlc_1988_2015.txt"

dd <- load_hurricane(workdir, data_file)
df_ike <- select_tracks(dd,storm_id = "IKE-2008", dt="2008-09-13", tm="12:00:00")

us_map <- ggplot2::map_data("state")

base_map <- ggplot() +
  geom_polygon(data = us_map, aes(x = long, y = lat, group = group)) +
  theme(legend.position="none") +
  xlim(c(-125, -70)) + ylim(c(20, 50))

p1 <- base_map + geom_hurricane(data = df_ike, aes(x = Long, y = Lat,
                                             r_ne = ne, r_se = se, r_nw = nw, r_sw = sw,
                                             fill = as.character(wind_speed),
                                             col = as.character(wind_speed)),
                          scale_radii = 0.8) +
  theme(legend.justification=c(1,0), legend.position=c(0.97,0.03)) +
  scale_color_manual(name = "Wind speed (kts)",
                     values = c("red", "orange", "yellow")) +
  scale_fill_manual(name = "Wind speed (kts)",
                    values = c("red", "orange", "yellow")) +
  theme(legend.position = "bottom")

ggplot2::ggsave(filename = "output_file.png", device = "png", dpi = 720,
                width = 16, height = 9)


