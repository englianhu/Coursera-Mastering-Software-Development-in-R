#' A new ggproto class to support the geom_hurricane ggplot function
#' #' Completed as part of the "Building Data Visualisation Tools" coursera course
#' 
#' @param required_aes aes input configuration
#' @param default_aes aes default configuration
#' @param draw_key legend function
#' @param draw_group plot implementation
#' @param setup_data setup function for scaling
#' 
#' @export
GeomHurricane <- ggplot2::ggproto("GeomHurricane", 
                                  Geom, 
                                  required_aes = c("x", "y",
                                                   "r_ne", "r_se", "r_sw", "r_nw"
                                  ),
                                  default_aes = aes(fill = 1, colour = 1, 
                                                    alpha = 0.5, scale_radii = 1),
                                  draw_key = draw_key_polygon,
                                  
                                  draw_group = function(data, panel_scales, coord) {
                                    
                                    data <- dplyr::mutate(data,
                                             r_ne = r_ne * 1852 * scale_radii, 
                                             r_se = r_se * 1852 * scale_radii, 
                                             r_sw = r_sw * 1852 * scale_radii,
                                             r_nw = r_nw * 1852 * scale_radii)
                                    
                                    # Creating quadrants 
                                    for(i in 1:nrow(data)) {
                                      
                                      # Creating the northeast quadrants
                                      data_ne <- data.frame(colour = data[i,]$colour,
                                                            fill = data[i,]$fill,
                                                            geosphere::destPoint(p = c(data[i,]$x, data[i,]$y),
                                                                                 b = 1:90,
                                                                                 d = data[i,]$r_ne),
                                                            group = data[i,]$group,
                                                            PANEL = data[i,]$PANEL,
                                                            alpha = data[i,]$alpha
                                      )
                                      
                                      # Creating the southeast quadrants
                                      data_se <- data.frame(colour = data[i,]$colour, 
                                                            fill = data[i,]$fill,
                                                            geosphere::destPoint(p = c(data[i,]$x, data[i,]$y),
                                                                                 b = 90:180,
                                                                                 d = data[i,]$r_se),
                                                            group = data[i,]$group,
                                                            PANEL = data[i,]$PANEL,
                                                            alpha = data[i,]$alpha
                                      )
                                      
                                      # Creating the southwest quadrants
                                      data_sw <- data.frame(colour = data[i,]$colour, 
                                                            fill = data[i,]$fill,
                                                            geosphere::destPoint(p = c(data[i,]$x, data[i,]$y),
                                                                                 b = 180:270,
                                                                                 d = data[i,]$r_sw),
                                                            group = data[i,]$group,
                                                            PANEL = data[i,]$PANEL,
                                                            alpha = data[i,]$alpha
                                      )
                                      
                                      # Creating the northwest quadrants
                                      data_nw <- data.frame(colour = data[i,]$colour,
                                                            fill = data[i,]$fill, 
                                                            geosphere::destPoint(p = c(data[i,]$x, data[i,]$y),
                                                                                 b = 270:360,
                                                                                 d = data[i,]$r_nw),
                                                            group = data[i,]$group,
                                                            PANEL = data[i,]$PANEL,
                                                            alpha = data[i,]$alpha
                                      )
                                      
                                      data_quadrants <- dplyr::bind_rows(list(
                                        data_ne, data_se, data_sw, data_nw
                                      )) 
                                      
                                      data_quadrants <- dplyr::rename(data_quadrants, c(x = lon, y = lat))
                                      
                                      data_quadrants$colour <- as.character(data_quadrants$colour)
                                      data_quadrants$fill <- as.character(data_quadrants$fill)
                                      
                                    }
                                    
                                    coords_data <- coord$transform(data_quadrants, panel_scales)
                                    
                                    grid::polygonGrob(
                                      x = coords_data$x,
                                      y = coords_data$y,
                                      default.units = "native", 
                                      gp = grid::gpar(
                                        col = coords_data$colour, 
                                        fill = coords_data$fill,
                                        alpha = coords_data$alpha
                                      )
                                    )
                                  },
                                  
                                  setup_data = function(data, params) {
                                    
                                    maxrad <- max(c(data$r_ne, data$r_se, data$r_sw, data$r_nw))
                                    maxrad <- maxrad * 1852
                                    
                                    x_range <- unique(range(data$x))
                                    y_range <- unique(range(data$y))
                                    xy <- as.matrix(expand.grid(x_range, y_range))
                                    
                                    extend <- lapply(c(0, 90, 180, 270), function(b) {
                                      geosphere::destPoint(p = xy,
                                                           b = b,
                                                           d = maxrad)
                                    })
                                    extend <- do.call(rbind, extend)
                                    
                                    transform(
                                      data,
                                      xmin = min(extend[, 1]),
                                      xmax = max(extend[, 1]),
                                      ymin = min(extend[, 2]),
                                      ymax = max(extend[, 2])
                                    )
                                  }
)

#' GGplot Geom function to plot hurricane intensity at a given location and point in time.
#' 
#' The function draws a plot of hurricane wind intensity at a given X-Y location.
#' 
#' Completed as part of the "Building Data Visualisation Tools" coursera course
#'
#' @param mapping 
#' @param data 
#' @param stat 
#' @param position 
#' @param na.rm 
#' @param show.legend 
#' @param inherit.aes 
#'
#' @return None
#' @export
geom_hurricane <- function(mapping = NULL, data = NULL, stat = "identity",
                             position = "identity", na.rm = FALSE, 
                             show.legend = NA, inherit.aes = TRUE) {
  ggplot2::layer(
    geom = GeomHurricane, mapping = mapping,  
    data = data, stat = stat, position = position, 
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm)
  )
}


