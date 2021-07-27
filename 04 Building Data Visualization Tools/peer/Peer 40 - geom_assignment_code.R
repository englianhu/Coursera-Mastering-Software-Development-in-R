# library('tidyverse')
library('tidyr')
library('dplyr')

#' @title
#' Reads & wrangles the data from Fixed Width File
#'
#' @description
#' Reads the data from FWF file as per the specific format provided for
#' Extended Best Tract dataset.
#'
#' @param path Character Vector containing the location (path) of the file
#' @param fwfilename Character Vector having the name of the fwf file from which
#' contains the data in the specific fwf format of extended best tract dataset
#'
#' @return This function returns a data.frame having observations for wind radii
#' data which is prepared for the visualization of the storm structure in
#' directions of ne, se, sw, nw
#'
#' @importFrom readr read_fwf
#' @importFrom readr fwf_widths
#' @importFrom tibble as_tibble
#' @importFrom tidyr gather
#' @importFrom tidyr separate
#' @importFrom tidyr spread
#' @importFrom dplyr mutate
#'
#' @export
get_ext_tracks_tidy <- function(path, fwfilename){
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

  ext_tracks <- readr::read_fwf(paste0(path, fwfilename),
                                readr::fwf_widths(ext_tracks_widths, ext_tracks_colnames),
                                na = "-99")

  ext_tracks_tidy <- ext_tracks %>%
    tibble::as_tibble() %>%
    tidyr::gather(key=wind_speed, value=wind_radii, radius_34_ne:radius_64_nw) %>%
    tidyr::separate(wind_speed,c('cat','wind_speed','direction'),sep='\\_') %>%
    tidyr::spread(direction, wind_radii) %>%
    dplyr::mutate(storm_year_id=paste0(storm_name,'-',as.character(year))) %>%
    dplyr::mutate(storm_date=(paste0(as.character(year),'-',as.character(month),'-',as.character(day),' ',as.character(hour),':00:00'))) %>%
    dplyr::mutate(longitude = longitude*-1,
                  wind_speed = as.factor(wind_speed))

  ext_tracks_tidy <- ext_tracks_tidy[c(24:25,7:8,19:23)]
  ext_tracks_tidy
}

##################################

library(grid)
library(geosphere)

#' @title
#' Compute the points for the Geo Polygon
#'
#' @description
#' Takes the single observation having wind radii and returns a data frame with
#' all points required to generate geo ploygon in its 360 degrees having bearings
#' which reflects the nautical miles of wind speed
#'
#' @param obs Observation having longitude and latitude along with spread of
#' wind in ne, se, sw & nw directions
#' @param fill Color for the gew polygon fill
#' @param lcol Color for the border line of the polygon
#' @param scale The scale_radii value indicating the visualization extend
#'
#' @return This function returns a data.frame having data points for the geo
#' polygon that could be used to visualize the wind speed in the directions of
#' ne, se, sw & nw
#'
#' @importFrom geosphere destPoint
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
getpts_geo_polygon <- function(obs, fill, lcol, scale=1){
  if(nrow(obs)==1){
    convert2Mtrs <- 1852

    lon <- obs['longitude'][[1]]
    lat <- obs['latitude'][[1]]

    ne <- 0
    se <- 90
    sw <- 180
    nw <- 270

    ne_90 <- ne+90
    se_90 <- se+90
    sw_90 <- sw+90
    nw_90 <- nw+90

    ne_mtrs <- obs['ne']*convert2Mtrs*scale
    se_mtrs <- obs['se']*convert2Mtrs*scale
    sw_mtrs <- obs['sw']*convert2Mtrs*scale
    nw_mtrs <- obs['nw']*convert2Mtrs*scale

    pts <- data.frame(rbind(
        geosphere::destPoint(c(lon,lat), b=ne:ne_90, d=ne_mtrs),
        geosphere::destPoint(c(lon,lat), b=se:se_90, d=se_mtrs),
        geosphere::destPoint(c(lon,lat), b=sw:sw_90, d=sw_mtrs),
        geosphere::destPoint(c(lon,lat), b=nw:nw_90, d=nw_mtrs)
      )) %>%
      dplyr::mutate(
        'fill'=fill,
        'col'=lcol,
        'x'=lon,
        'y'=lat,
        'scale_radii'=obs['scale_radii'][[1]],
        'PANEL'=obs['PANEL'][[1]],
        'group'=obs['group'][[1]]
      )
  }
  else{
    pts <- data.frame('lon'=0,'lat'=0,'fill'=0,'col'=0,'x'=0,'y'=0,'scale_radii'=0,'PANEL'=0,'group'=0) %>%
      dplyr::filter('x'>0)
  }
  pts <- pts %>% filter(x!=0)

  pts
}

#' @title
#' Compute function used in the Stat to return the points for GeomHurricane
#'
#' @description
#' This function is used as the compute_group function of StatHurricane. For
#' each row found in the dataset passed to the geom, the stat is called to
#' compute the points which could be later drawn by the geom
#'
#' @param data The data row on which the computation is required in order to
#' visual it with the geom draw_panel function
#' @param scales The scales that are passed by the geom function caller
#'
#' @return This function returns the tibble of data frame that comprises of pts
#' which is then passed to the geom draw_panel to generate the visualizations
#' on the ggplot object
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom tibble as_tibble
compute_group_hurricane <- function(data, scales) {
  data <- data %>%
    dplyr::mutate('longitude'=x,
           'latitude'=y,
           'ne'=r_ne,
           'se'=r_se,
           'sw'=r_sw,
           'nw'=r_nw,
    )

  lon <- data[1,'longitude'][[1]]
  lat <- data[1,'latitude'][[1]]
  scale_radii <- data[1,'scale_radii'][[1]]

  if(is.null(scale_radii)){
    scale_radii <- 1
    data <- data %>%
      mutate(scale_radii=1)
  }


  convert2Mtrs <- 1852
  max_d <- 710*convert2Mtrs   #nautical miles

  pts1 <- data %>%
    dplyr::filter(
      fill=='34'
    ) %>%
    getpts_geo_polygon(
      adjustcolor("red",alpha.f=0.7),
      adjustcolor("red",alpha.f=1),
      scale_radii)

  pts2 <- data %>%
    dplyr::filter(
      fill=='50'
    ) %>%
    getpts_geo_polygon(
      adjustcolor("orange",alpha.f=0.7),
      "darkorange",
      scale_radii)

  pts3 <- data %>%
    dplyr::filter(
      fill=='64'
    ) %>%
    getpts_geo_polygon(
      adjustcolor("yellow",alpha.f=0.7),
      "yellow",
      scale_radii)

  pts <- data.frame(rbind(pts1,pts2,pts3))
  pts <- data.frame(pts['x'],pts['y'])

  tibble::as_tibble(pts)
}

#' @title
#' Draw Panel function as used in the GeomHurricane
#'
#' @description
#' This function returns a grid grob object that will be plotted on the ggplot
#' object (this is where the real work occurs for the custom geom). The custom
#' geom is inherited from the GeomPolygon hence this function is expected to
#' return a polygonGrob.
#'
#' @param data The complete data set that is required to visualize it the
#' hurricane data while extending the GeomPolygon class
#'
#' @param panel_scales The panel scales that are passed by the ggplot object to
#' the Geom layer
#'
#' @param coord The coord object which contains the transform function
#'
#' @return This function returns the graphic object (Grob) which can be plotted
#' as an additional layer on the ggplot object. Since our custom geom extends
#' the GeomPolygon class, the returned graphic object is polygonGrob.
#'
#' @importFrom tibble as_tibble
#' @importFrom tidyr drop_na
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise_all
#' @importFrom dplyr first
#' @importFrom grid polygonGrob
#' @importFrom grid gpar
#' @importFrom scales alpha
draw_panel_hurricane <- function(data, panel_scales, coord) {
  datapts <- tibble::as_tibble(data)
  datapts <- datapts %>%
    tidyr::drop_na()

  # print(data.frame(datapts) %>%
  #         dplyr::group_by(datapts$group) %>%
  #         dplyr::summarise_all(dplyr::first))

  ## Transform the data first
  coords <- coord$transform(datapts, panel_scales)

  firsts <- data.frame(coords) %>%
          dplyr::group_by(coords$group) %>%
          dplyr::summarise_all(dplyr::first)

  pg <- grid::polygonGrob(
    x=coords$x,
    y=coords$y,
    id=coords$group,
    default.units = 'native',
    gp = grid::gpar(col = firsts$colour,
                    fill = scales::alpha(firsts$fill, firsts$alpha),
                    alpha = firsts$alpha,
                    lty = "solid",
                    lwd = 4)
  )

  pg
}

##############
#' @describeIn stat_hurricane
#' @describeIn geom_hurricane
#'
#' @title
#' Constructs a new object with ggproto
#'
#' @description
#' This object shall be passed as default value for stat in stat_hurricane func
#'
#' @importFrom ggplot2 ggproto
#' @importFrom ggplot2 Stat
#' @importFrom ggplot2 aes
StatHurricane <- ggplot2::ggproto("StatHurricane",
                                  ggplot2::Stat,
                                  compute_group = compute_group_hurricane,
                                  required_aes = c("x", "y", "r_ne","r_se","r_sw","r_nw"),
                                  default_aes = ggplot2::aes(colour = "red")
)

#' @title
#' Adds new layer on ggplot that computes Hurricane Wind Radii polygon points
#'
#' @description
#' This function returns a ggplot layer object which performs required
#' computation on the data that could be passed to geom. Not used in current
#' scenario.
#'
#' @inheritParams ggplot2::layer mapping
#' @inheritParams ggplot2::layer data
#' @inheritParams ggplot2::layer geom
#' @inheritParams ggplot2::layer position
#' @inheritParams ggplot2::layer show.legend
#' @inheritParams ggplot2::layer inherit.aes
#' @params na.rm Additional Parameters to geom: remove na or not
#' @params outliers Additional Parameters to geom: outliers exclude or not
stat_hurricane <- function(mapping = NULL,
                           data = NULL,
                           geom = "polygon",
                           position = "identity",
                           show.legend = NA,
                           inherit.aes = TRUE,
                           na.rm = FALSE,
                           outliers = TRUE,
                           ...) {
  ggplot2::layer(
    stat = StatHurricane,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm,
                  outliers = outliers
                  , ...)
  )
}

##############
#' @describeIn geom_hurricane
#'
#' @title
#' Constructs a new object with ggproto
#'
#' @description
#' This object shall be passed as default value for geom in geom_hurricane func
#'
#' @details AES Parameters
#' REQUIRED
#' x,y: The x & y coordinates for drawing the polygon grob
#'
#' r_ne,r_se,r_sw,r_nw: The speed of wind in the directions of ne, se, sw, nw
#'
#' fill: The color used to fill the polygon
#'
#'
#' OPTIONAL
#' scale_radii: The scale of the hirrical_wind_radii. E.g 1=100%; 0.5=50%
#'
#' colour: The color of the polygon border. Default is green color
#'
#' alpha: The transparency of fill color. Default is 0.6#'
#'
#' @importFrom ggplot2 ggproto
#' @importFrom ggplot2 GeomPolygon
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 draw_key_polygon
GeomHurricane <- ggplot2::ggproto("GeomHurricane",
                                  ggplot2::GeomPolygon,
                                  required_aes = c("x","y",
                                                   "r_ne","r_se","r_sw","r_nw",
                                                   "fill"),
                                  default_aes = ggplot2::aes(scale_radii = 1,
                                                             colour = "green",
                                                             alpha = 0.6,
                                                             lwd=1)
                                  ,draw_key = ggplot2::draw_key_polygon
                                  ,draw_panel = draw_panel_hurricane
                                  )

#' @title
#' Adds new layer on ggplot that computes Hurricane Wind Radii polygon points
#'
#' @description
#' This function returns a ggplot layer object which draws the geo polygon
#' points in order to show the hurricane wind radii for single observation.
#'
#' @inheritParams ggplot2::layer mapping
#' @inheritParams ggplot2::layer data
#' @inheritParams ggplot2::layer position
#' @inheritParams ggplot2::layer show.legend
#' @inheritParams ggplot2::layer inherit.aes
#' @params na.rm Additional Parameters to geom: remove na or not
geom_hurricane <- function(mapping = NULL,
                           data = NULL,
                           position = "identity",
                           show.legend = NA,
                           inherit.aes = TRUE,
                           na.rm = FALSE,
                           ...) {
  ggplot2::layer(
    geom = GeomHurricane,
    mapping = mapping,
    data = data,
    stat = StatHurricane,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

library(ggmap)

#######################
#' @title
#' Adds new layer on ggplot that computes Hurricane Wind Radii polygon points
#'
#' @description
#' This function returns a ggplot layer object which draws the geo polygon
#' points in order to show the hurricane wind radii for single observation.
#'
#' @inheritParams get_ext_tracks_tidy path
#' @inheritParams get_ext_tracks_tidy fwfilename
#' @params hurricane_year_id The STORM id and the YEAR, separated by hyphen
#' @params hurricane_date The date and time in format yyyy-mm-dd HH:MM:SS
#' @params na.rm Additional Parameters for remove na or not
#' @params google_key The API Key to be used for Google Apis to get the map
#' @params map_location The location for which map to used at background
#' @params map_zoom Value of ZOOM to be used while fetching the map
#'
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom ggmap register_google
#' @importFrom ggmap get_map
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 facet_wrap
#' @importFrom ggplot2 scale_color_manual
#' @importFrom ggplot2 scale_fill_manual#'
plot_hurricane <- function(path,
                           fwfilename="ebtrk_atlc_1988_2015.txt",
                           hurricane_year_id='KATRINA-2005',
                           hurricane_date='2005-08-29 12:00:00',
                           na.rm=TRUE,
                           google_key=NULL,
                           map_location="Louisiana",
                           map_zoom=6){
  katrina <- get_ext_tracks_tidy(path, fwfilename) %>%
    dplyr::filter(storm_year_id==hurricane_year_id,
                  storm_date==hurricane_date,
                  na.rm==na.rm)
  katrina_withradii <- rbind(katrina %>%
                               dplyr::mutate(radii=1,
                                             radii_display='Scale Radii 100%'),
                             katrina %>%
                               dplyr::mutate(radii=0.5,
                                             radii_display='Scale Radii 50 %'))
  print(katrina_withradii)
  # storm_year_id storm_date latitude longitude wind_speed    ne    nw    se    sw radii
  # <chr>         <chr>         <dbl>     <dbl> <fct>      <dbl> <dbl> <dbl> <dbl> <dbl>
  # 1 KATRINA-2005  2005-08-2~     29.5     -89.6 34           200   100   200   150   1
  # 2 KATRINA-2005  2005-08-2~     29.5     -89.6 50           120    75   120    75   1
  # 3 KATRINA-2005  2005-08-2~     29.5     -89.6 64            90    60    90    60   1
  # 4 KATRINA-2005  2005-08-2~     29.5     -89.6 34           200   100   200   150   0.5
  # 5 KATRINA-2005  2005-08-2~     29.5     -89.6 50           120    75   120    75   0.5
  # 6 KATRINA-2005  2005-08-2~     29.5     -89.6 64            90    60    90    60   0.5

  ## INITIAL TEST - GEOM CODE
  ## (Uncomment below code and run)
  # print(katrina)
  # grid.draw(
  #   ggplot(data = katrina) +
  #     geom_hurricane(aes(x = longitude, y = latitude,
  #                        r_ne = ne, r_se = se, r_nw = nw, r_sw = sw,
  #                        fill = wind_speed, color = wind_speed)) +
  #     scale_color_manual(name = "Wind speed (kts)",
  #                        values = c("red", "orange", "yellow")) +
  #     scale_fill_manual(name = "Wind speed (kts)",
  #                       values = c("red", "orange", "yellow"))
  # )

  ## GEOM VALIDATION ON MAP
  if(is.null(google_key))
  {
    print('Assumption: Key already applied on ggmap::register_google')
  }
  else
    ggmap::register_google(key=google_key)

  storm_observation <- katrina_withradii
  grid.draw(
    ggmap::get_map(map_location,
                   zoom = map_zoom,
                   maptype = "toner-background",
                   source = "stamen") %>%
      ggmap::ggmap(extent = "device") +
      geom_hurricane(data = storm_observation,
                     ggplot2::aes(x = longitude,
                                  y = latitude,
                                  r_ne = ne,
                                  r_se = se,
                                  r_nw = nw,
                                  r_sw = sw,
                                  fill = wind_speed,
                                  colour = wind_speed,
                                  scale_radii = radii)
      ) +
      ggplot2::facet_wrap(~ radii_display, ncol = 2) +
      ggplot2::scale_color_manual(name = "Wind speed (kts)",
                                  values = c("red", "orange", "yellow")) +
      ggplot2::scale_fill_manual(name = "Wind speed (kts)",
                                 values = c("red", "orange", "yellow"))
  )
}

#############################
plot_hurricane(path="~/RFiles/geom/data/")
print('Map Plotted Successully')

