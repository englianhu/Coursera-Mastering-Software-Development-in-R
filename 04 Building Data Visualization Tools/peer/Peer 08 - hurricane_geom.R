# Should not be included if this was a proper module
# but this makes it so the file can be sourced.
library(geosphere)
library(ggplot2)

#' Wind radii chart
#'
#' The chart is composed of four quadrants per wind radii where
#' each quadrant has a wind speed distribution.
#' 
#' @seealso
#'   [geom_polygon()] for a filled polygon
#' @export
#' @inheritParams layer
#' @inheritParams geom_polygon
#' @param scale_radii Scale of chart where 1.0 is scale 1:1
#' (one meter per meter.)
#' @examples
#' # When using geom_hurricane, you will typically use a data frame in the
#' # following format:
#' #
#' #       storm_id                date latitude longitude wind_speed  ne  nw se  sw
#' # 1 Katrina-2005 2005-08-29 12:00:00     29.5     -89.6         34 200 100 200 150
#' # 2 Katrina-2005 2005-08-29 12:00:00     29.5     -89.6         50 120  75 120  75
#' # 3 Katrina-2005 2005-08-29 12:00:00     29.5     -89.6         64  90  60 90  60
#' #
#' # per the following documentation:
#' # https://rammb.cira.colostate.edu/research/tropical_cyclones/tc_extended_best_track_dataset/docs/ebtrk_readme.txt
#'
#' ggmap::get_map(
#'   "Galveston, Texas",
#'   zoom = 6,
#'   source = "stamen",
#'   maptype = "toner-background"
#'   ) %>%
#'     ggmap::ggmap(extent = "device") +
#'     geom_hurricane(
#'       data = storm_observation,
#'       ggplot2::aes(
#'         x = longitude,
#'         y = latitude, 
#'         r_ne = ne,
#'         r_se = se,
#'         r_nw = nw,
#'         r_sw = sw,
#'         fill = wind_speed,
#'         color = wind_speed
#'       ),
#'       scale_radii = 1.0
#'     ) + 
#'     ggplot2::scale_color_manual(
#'       name = "Wind speed (kts)", 
#'       values = c("red", "orange", "yellow")
#'     ) + 
#'     ggplot2::scale_fill_manual(
#'       name = "Wind speed (kts)", 
#'       values = c("red", "orange", "yellow")
#'     ) +
#'     ggplot2::labs(
#'       title = "Hurricane Ike hits Galveston, Texas on Sept 13, 2008"
#'     )
#'
geom_hurricane <- function(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  scale_radii = 1,
  show.legend = NA,
  inherit.aes = TRUE,
  ...
) {
  ggplot2::layer(
    geom = GeomHurricane,
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(scale_radii = scale_radii, ...)
  )
}

# Although exported, this is how scarce this is documented in ggplot's
# geom_polygon so I assume this is enough here too. I guess if this is
# going to be used directly, one would have to look at the source code
# regardless.
#' 
#' @format NULL
#' @usage NULL
#' @export
GeomHurricane <- ggplot2::ggproto(
  "GeomHurricane",
  ggplot2::Geom,
  extra_params = c("scale_radii"),
  required_aes = c("x", "y", "r_ne", "r_se", "r_sw", "r_nw"),
  default_aes = aes(
    fill = "grey20",
    colour = "black",
    size = 0.5,
    alpha = 0.6
  ),
  draw_key = ggplot2::draw_key_point,
  draw_panel = function(
    data,
    panel_scales,
    coord,
    na.rm = FALSE,
    scale_radii = 1
  ) {
    # Inner function for creating a dataframe per polygon.
    # The returned data corresponds to the input expected by
    # the GeomPolygon class method `draw_panel().
    create_polygon <- function(id, x, y, ne, se, sw, nw) {
      # Note the overlap at the edges of the quadrants.
      n_points = 30
      s <- c(
        Map(c, seq(0, 90, length.out = n_points), ne),
        Map(c, seq(90, 180, length.out = n_points), se),
        Map(c, seq(180, 270, length.out = n_points), sw),
        Map(c, seq(270, 360, length.out = n_points), nw)
      )
      polygon_data <- mapply(
        function(bearing_radii, x, y, id) {
          lonlat <- geosphere::destPoint(
            c(x, y),
            b = bearing_radii[1],
            d = bearing_radii[2] * scale_radii
          )
          c(bearing_radii[1], lonlat[1], lonlat[2], id)
        },
        bearing_radii = s,
        MoreArgs = list(x = x, y = y, id = id)
      )
      data.frame(
        id = polygon_data[1,],
        x = polygon_data[2,],
        y = polygon_data[3,],
        group = polygon_data[4,]
      )
    }
    
    polygons <- lapply(
      seq_len(nrow(data)),
      function(i) {
        polys <- create_polygon(
          i,
          data$x[i],
          data$y[i],
          data$r_ne[i],
          data$r_se[i],
          data$r_sw[i],
          data$r_nw[i]
        )
        cbind(polys, unclass(data[i, c("fill", "colour", "size", "alpha")]))
      }
    )
    polygons <- do.call(rbind, polygons)
    
    # Instead of creating our own grob, we'll reuse the polygon geom to plot.
    ggplot2:::ggname(
      "geom_hurricane", 
      ggplot2::GeomPolygon$draw_panel(
        data = polygons,
        panel_params = panel_scales,
        coord = coord
      )
    )
  }
)
