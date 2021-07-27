#' @title read_ext_tracks
#'
#' @description Read in ext_tracks data from Colorado State
#'
#'
#' @param file Filename or raw data
#'
#' @param widths Vector of columns widths
#'
#' @param colnames Character vector of column names
#'
#' @param degW Degrees West, file coding format for nothern hemisphere.
#'
#' @param ... Alternative arguments to read_fwf()
#'
#'
#' @inheritParams readr::read_fwf
#'
#' @return A data.frame
#'
#' @importFrom readr read_fwf
#'
#' @export
#'

read_ext_tracks<-function(file="ebtrk_atlc_1988_2015.txt",...,widths=NULL,
                          colnames=NULL,degW=TRUE){
        if (is.null(widths)){
                widths <- c(7, 10, 2, 2, 3, 5, 5, 6, 4, 5, 4, 4, 5, 3, 4, 3, 3, 3,
                            4, 3, 3, 3, 4, 3, 3, 3, 2, 6, 1)
        }

        if (is.null(colnames)){
                colnames <- c("storm_id", "storm_name", "month", "day",
                              "hour", "year", "latitude", "longitude",
                              "max_wind", "min_pressure", "rad_max_wind",
                              "eye_diameter", "pressure_1", "pressure_2",
                              paste("radius_34", c("ne", "se", "sw", "nw"), sep = "_"),
                              paste("radius_50", c("ne", "se", "sw", "nw"), sep = "_"),
                              paste("radius_64", c("ne", "se", "sw", "nw"), sep = "_"),
                              "storm_type", "distance_to_land", "final"
                )
        }

        ext_tracks <- readr::read_fwf(file,
                                      readr::fwf_widths(widths, colnames),
                                      na = "-99")

        #Change sign of longitude
        if (degW) {ext_tracks$longitude<- -ext_tracks$longitude }

        return(ext_tracks)
}


#' @title tidy_tracks
#'
#' @description Tidy data creation for hurricane data. It takes data from the
#' read_ext_tracks
#'
#'
#' @param ext_tracks Output from the read_ext_tracks function.
#'
#' @param makeDate Date columns combination option into a single date column.
#'
#' @param keepcols Character vector of the columns  retained from the original data
#' #'
#' @return A tidy data.frame
#'
#' @importFrom dplyr contains
#' @importFrom dplyr select
#' @importFrom tidyr gather
#' @importFrom tidyr spread
#' @importFrom tidyr separate
#' @importFrom lubridate ymd_h
#' @importFrom stats setNames
#'
#' @export

tidy_tracks<-function(ext_tracks,makeDate=TRUE,
                      keepcols=c("storm_name","date","longitude","latitude")){

        # Adding extra columns
        keepcols<-c(keepcols,c("quadrant","wind_speed","wind_radius"))
        # Gathering radius columns into a single column
        gather_cols<-names(ext_tracks)[dplyr::contains(match="radius_",
                                vars=names(ext_tracks))]

        tidy_tracks<-tidyr::gather_(ext_tracks,key_col="speed_quadrant",
                                value_col="wind_radius",gather_cols=gather_cols,
                                na.rm=FALSE,factor_key=FALSE)

        tidy_tracks<-tidyr::separate_(tidy_tracks,col="speed_quadrant",
                                 into=c("drop","wind_speed","quadrant"),
                                 sep="_",remove=TRUE,convert=TRUE)
        # Nulling unusued name column
        tidy_tracks$drop<-NULL


        #create a single time field for ease of equivalence checking
        if (makeDate){tidy_tracks$date<-lubridate::ymd_h(paste(tidy_tracks$year,
                tidy_tracks$month, tidy_tracks$day,tidy_tracks$hour,sep=""))
        }


        tidy_tracks<-dplyr::select_(tidy_tracks,.dots=stats::setNames(keepcols,keepcols))

        return(tidy_tracks)
}


#'@title hurricane_geocode
#'
#' @description Storm center and wind radius conversion to a set of arc points
#'
#' @param storm_data Data set from tidy_tracks.
#'
#' @param x longitude coordinate of the eye of the hurricanes.
#'
#' @param y Latituted coordinate of the eye of the hurricanes.
#'
#' @param r Farthest distance for which the wind speed was recorded.
#'
#' @param quadrant spatial quadrants on a map (ne, se, sw, and nw) .
#'
#' @param wind_speed Wind speed measure.
#'
#' @param arcRes Angular degrees'
#'
#' @return A data.frame.
#'
#' @importFrom geosphere destPoint
#' @importFrom dplyr filter_
#' @importFrom dplyr select_
#' @importFrom dplyr arrange_
#' @importFrom dplyr bind_rows
#' @importFrom dplyr rename_
#'
#' @export

hurricane_geocode<-function(storm_data,x="longitude",y="latitude",r="wind_radius",
                            quadrant="quadrant",wind_speed="wind_speed",
                            arcRes=1){

        # Renaming arguments
        storm_data<-dplyr::rename_(storm_data, .dots=stats::setNames(list(x,y,r,
                        quadrant,wind_speed), c("x","y","r","quadrant","wind_speed")))

        # Merging data needed for geodesic inputs with each record
        arc_data<-dplyr::data_frame(quadrant=c("ne","se","sw","nw"),
                                    start_angle=c(0,90,180,270),
                                    end_angle=c(90,180,270,360))

        merged_data<-merge(storm_data,arc_data,by="quadrant")


        # Points for polygon for each speed and the speed above it.
        speeds<-unique(merged_data$wind_speed)
        for (s in 1:length(speeds)){

                arc_data<-dplyr::filter_(merged_data,
                                         ~wind_speed %in% c(speeds[s],speeds[s+1]))

                for (a in 1:nrow(arc_data)){

                        if(arc_data$wind_speed[a]==speeds[s]){
                                arc_data$radtype[a]<-"outer"
                        }else arc_data$radtype[a]<-"inner"

                        ad_p<-c(arc_data$x[a],arc_data$y[a])
                        ad_d<-arc_data$r[a]*1852


                        # Sequence of angles;
                        ad_s<-unique(c(seq(arc_data$start_angle[a],
                                           arc_data$end_angle[a],
                                           by=arcRes),arc_data$end_angle[a]))

                        # Geocode arcs for plotting
                        points<-data.frame(geosphere::destPoint(p=ad_p,
                                                                d=ad_d, b=ad_s))
                        points$angle<-ad_s

                        points[,names(arc_data)]<-arc_data[a,]
                        points$wind_speed<-speeds[s]
                        if (s==1&&a==1) out<-points
                        else out<-dplyr::bind_rows(out,points)
                }
        }

        # Ordering and removal collection of points at 0 (radius==zero)

        out$quadrant<-factor(out$quadrant,levels=c("ne","se","sw","nw"))
        out<-dplyr::arrange_(out,~wind_speed,~radtype,~quadrant,~angle)
        out<-dplyr::filter_(out,~r>0)

        # Renaming coordinates

        out<-dplyr::rename_(out,.dots=stats::setNames(list(~x,~y),
                                                      c("eye_lon","eye_lat")))
        out<-dplyr::rename_(out,.dots=stats::setNames(list(~lon,~lat),c(x,y)))

        # cleaning extra variables

        out$start_angle<-NULL
        out$end_angle<-NULL
        out$quadrant<-as.character(out$quadrant)

        return(out)
}

#' @title GeomHurricane
#' @description
#' ggproto object to display the hurricane radii
#' plots.
#
#' @param arc_step Resolution of the arcs in the wind_radii graph.
#'
#' @param scale_radii Implement to scale the radius size.
#'
#' @import ggplot2
#' @import grid
#' @importFrom dplyr select
#' @importFrom dplyr left_join
#'
#' @export

GeomHurricane<-ggplot2::ggproto("GeomHurricane",GeomPolygon,
                               required_aes = c("x","y","r","quadrant","wind_speed"
                                ),
                                default_aes = aes(fill="red",
                                                  colour="red",
                                                  size=0.5,
                                                  linetype=1,
                                                  alpha=.5,
                                                  arc_step=1,
                                                  scale_radii=1
                                ),

# Data tranformation

draw_panel = function(self,data,panel_scales,coord){
                extravars<-dplyr::select_(data,~-x,~-y,~-r)
                data0<-data
                data0$r<-data0$r*data0$scale_radii

# Geocoded points generation
data0<-hurricane_geocode(storm_data=data0, x="x", y="y", r="r",
                wind_speed="wind_speed", quadrant="quadrant",
                arcRes=data0$arc_step[1])

# Merging aestetics mapping


data0<-dplyr::select_(data0,~x,~y,~wind_speed,~quadrant)

extravars$quadrant<-factor(extravars$quadrant,levels=c("ne","se","sw","nw"))

extravars$wind_speed<-as.factor(extravars$wind_speed)

data0$quadrant<-factor(data0$quadrant, levels=c("ne","se","sw","nw"))

data0$wind_speed<-as.factor(data0$wind_speed)

data0<-dplyr::left_join(data0,extravars, by=c("wind_speed","quadrant"))

# Smoothing the polygons
data0$group<-data0$wind_speed

ggplot2:::ggname("geom_polygon",GeomPolygon$draw_panel(data0,
                panel_scales, coord))
})

#' @title geom_hurricane
#'
#' @description Wrapper to the layer function for the GeomHurricane.
#'
#'
#' @inheritParams ggplot2::geom_polygon
#'
#'
#'
#' @export
geom_hurricane<-function (mapping = NULL,
                          data = NULL,
                          stat = "identity",
                          position = "identity",
                          ..., na.rm = FALSE, show.legend = NA, inherit.aes = TRUE){

        layer(data = data, mapping = mapping, stat = stat,
              geom = GeomHurricane,
              position = position, show.legend = show.legend, inherit.aes = inherit.aes,
              params = list(na.rm = na.rm,
                            ...))
}
.............................................testing the code..........................................................................
# Since july 2018, using ggmap will fail unless you use Google Maps API Key. You will need to give Google your billing information, 
# but you can create maps for free: more infos https://www.visibledata.co.uk/blog/2018/12/05/2018-12-05-using-ggmap-after-july-2018/ 
Normal<-get_map(location=loc,
           zoom =6 , maptype = "toner-background") %>%
        ggmap(extent = "device")+geom_hurricane(data=storm_observation,
                                                mapping=aes(x=longitude,y=latitude,
                                                            r=wind_radius,
                                                            wind_speed=wind_speed,
                                                            quadrant=quadrant,
                                                            fill=as.factor(wind_speed),
                                                            color=as.factor(wind_speed)))+
        scale_color_manual(name = "Wind speed (kts)", 
                           values = c("red", "orange", "yellow")) + 
        scale_fill_manual(name = "Wind speed (kts)", 
                          values = c("red", "orange", "yellow"))+
        ggtitle("scale_radii=1.00")
Half<-get_map(location=loc,
            zoom =6 , maptype = "toner-background") %>%
        ggmap(extent = "device")+geom_hurricane(data=storm_observation,
                                                mapping=aes(x=longitude,y=latitude,
                                                            r=wind_radius,
                                                            wind_speed=wind_speed,
                                                            quadrant=quadrant,
                                                            fill=as.factor(wind_speed),
                                                            color=as.factor(wind_speed),
                                                            scale_radii=0.5))+
        scale_color_manual(name = "Wind speed (kts)", 
                           values = c("red", "orange", "yellow")) + 
        scale_fill_manual(name = "Wind speed (kts)", 
                          values = c("red", "orange", "yellow")) +
        ggtitle("scale_radii = 50%")
gridExtra::grid.arrange(Normal,Half,ncol=2,nrow=1)
