library(ggplot2)

StatHurricane <- ggproto("StatHurricane", Stat,
                         compute_group = function(data, scales, scale_radii) {
                           
                           require(geosphere)
                           
                           center=c(data$x,data$y)
                           
                           unit_conversion = 1852 #meters per nautical mile 
                           scale_factor <- scale_radii*unit_conversion
                           
                           bearings <- 0:90
                           radius = scale_factor*data$r_ne
                           NE <- data.frame(geosphere::destPoint(center, b=bearings, d=radius))
                           
                           bearings <- 90:180
                           radius = scale_factor*data$r_se
                           SE <- data.frame(geosphere::destPoint(center, b=bearings, d=radius))
                           
                           bearings <- 180:270
                           radius = scale_factor*data$r_sw
                           SW <- data.frame(geosphere::destPoint(center, b=bearings, d=radius))
                           
                           bearings <- 270:360
                           radius = scale_factor*data$r_nw
                           NW <- data.frame(geosphere::destPoint(center, b=bearings, d=radius))
                           
                           out <- rbind(NE,SE,SW,NW)
                           names(out) <- c("x","y")
                           out
                           
                         },
                         
                         required_aes = c("x", "y",
                                          "r_ne","r_se",
                                          "r_sw","r_nw")
)


#' Custom geom function for plotting wind radii diagrams.
#'
#' @param mapping Set of aesthetic mappings created by aes().
#' @param data A data.frame object containing the wind radii data to be displayed. 
#' The data.frame object must contain a column containing all wind speeds to be 
#' plotted. This includes a column containing the windspeed cutoffs and four 
#' additional columns, one for each quadrant (ne, se, sw, nw), holding the maximum 
#' radii for which the corresponding windspeed cutoff values were observed.  
#' In addition, two columns, one for longitude and one for latitude, which hold 
#' the position of the storm in terms of longitude and latitude are required.  
#' @param stat Character string specifying the transformation applied to the data 
#' before plotting. You must specify "hurricane" to create a wind radii diagram.
#' @param position Character string specifying the position adjustment, For this 
#' function "identity" is the default and most appropriate stat.
#' @param rule A character string, either "evenodd" or "winding", specifying the 
#' rule used for filling the shapes (polygons) that make up the wind radii diagram. 
#' The "winding" rule will work, but is for polygons with holes which are not 
#' relevant here.
#' @param ... Other arguments passed to ggplot2::layer()
#' @param na.rm Logical value which removes missing values with a warning if FALSE. 
#' If TRUE, removes missing values without warning.
#' @param show.legend Logical value which determines if a legend is generated 
#' for this layer.
#' @param inherit.aes Logical value 
#' @param alpha A numerical value between 0 and 1 which determines the transparency 
#' of the wind radii diagram. Default: alpha=0.5.
#' @param scale_radii A numerical value between 0 and 1 which can be used to scale
#' the wind radii diagram down, e.g. to fit on a map on which it is being overlaid.
#' Default: scale_radii=1.
#'
#' @note Required aesthetics: x, y, r_ne, r_se, r_sw, r_nw. Map x and y to 
#' longitude and latitude, respectively. Map r_ne to the column of wind radii for
#' the northeast quadrant of the storm. Likewise for r_se, r_sw, and r_nw.
#' @note Optional aesthetics: color, fill, alpha, scale_radii. color and fill 
#' should be mapped to the wind speed cutoffs column if desired.
#'
#' @export
#'
#' @examples
#' library(ggmap)
#' storm_observation <- data.frame(storm_id="IKE-2008",
#'                                 date = "2005-08-29 12:00:00",
#'                                 latitude = 30.3, longitude = -95.2,
#'                                 wind_speed = c("34","50","64"),
#'                                 ne = c(125,75,50), nw = c(60,45,20),
#'                                 se = c(180,90,45), sw = c(125,60,30) )
#' p <- get_map("Louisiana", zoom = 6, maptype = "toner-background")
#' p %>%  ggmap(extent = "device") +
#'   geom_hurricane(data= storm_observation, aes(x = longitude, y = latitude,
#'                                               r_ne = ne, r_se = se, r_nw = nw, r_sw = sw,
#'                                               color = wind_speed, fill=wind_speed), scale_radii = 0.8) +
#'   scale_color_manual(name = "Wind speed (kts)",
#'                      values = c("red", "orange", "yellow")) +
#'   scale_fill_manual(name = "Wind speed (kts)",
#'                     values = c("red", "orange", "yellow"))
#'                     
geom_hurricane <- function (mapping = NULL, data = NULL, stat = "hurricane", 
                            position = "identity", rule = "evenodd", ..., 
                            na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, alpha =0.5, scale_radii=1) 
{
  ggplot2::layer(data = data, mapping = mapping, stat = stat, geom = GeomPolygon, 
                 position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
                 params = list(na.rm = na.rm, rule = rule, alpha = alpha, scale_radii=scale_radii, ...))
}


