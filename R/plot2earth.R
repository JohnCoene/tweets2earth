#' plot2earth
#' 
#' @description
#' 
#' Plots tweets location and created time on Google Earth.
#' 
#' @param df Required. data.frame of tweets to plot.
#' @param longitude Required. Column holding longitude.
#' @param latitude Required. Column holding latitude.
#' @param date.time Required. Column including creation time of tweets; 
#' \code{POSIXct} format
#' @param shape, optional. Shape to use for points. Defaults to 
#' \code{http.../icon18.png}
#' @param labels Required. Column holding labels.
#' @param size Required. Column holding desired size of points.  
#' @param colour, optional. Column name that determines colours of points.
#' @param open, optional. Whether to open file in Google Earth. 
#' Defaults to \code{TRUE}.
#' @param file.name Name of kml or kmz file, defaults to \code{tweets2earth.kml}
#' @param folder.name Name of folder where to save the kml or kmz file, 
#' defaults to \code{getwd}
#' @param ... Arguments to pass to \code{kml} function. 
#' See \code{\link[plotKML]{kml}}
#' @param kmz Whether to also save the file as \code{.kmz}, defaults to 
#' \code{FALSE}
#' 
#' @details
#' 
#' Takes data.frame of tweets and plots coordinates on Google Earth, also 
#' includes time variable (\code{date.time}). Choose \code{size}, 
#' \code{colour} and \code{labels} of your liking.
#' 
#' @return 
#' 
#' Writes kml file and, optionally, opens it in Google Earth 
#' (\code{open = TRUE}).
#' 
#' @examples
#' 
#' \dontrun{
#' library(twitteR)
#' 
#' # setup oauth 
#' setup_twitter_oauth(consumer_key, consumer_secret, access_token=your_token, 
#'                     access_secret=your_secret)
#' 
#' # search twitter
#' tweets <- searchTwitter("rstats", n = 2000, lang= "en",
#'                         geocode="34.052235,-118.243683,100mi")
#' 
#' # unlist
#' tw.df <- do.call("rbind", lapply(tweets, as.data.frame))
#' 
#' # make file
#' earth <- plot2earth(tw.df, "longitude", "latitude", "created", 
#'                     "screenName", "retweetCount", "favorited", 
#'                     colour.scale = RColorBrewer::brewer.pal(8, "BuPu"),
#'                     file.name = "rstats.kml")
#' 
#' # plot            
#' plotKML::plotKML(earth, colour_scale = pal)
#' }
#' 
#' @author John Coene, \email{jcoenep@@gmail.com}
#' 
#' @seealso \\code{\link{stream2earth}}
#' 
#' @export
plot2earth <- function(df, longitude, latitude, date.time, labels, size, colour, 
                       shape = "http://maps.google.com/mapfiles/kml/pal2/icon18.png",
                       open = TRUE, kmz = FALSE, file.name = "tweets2earth.kml",
                       folder.name = getwd(), ...) {
  
  if(missing(df)) stop("must pass df")
  if(missing(longitude)) stop("must pass longitude")
  if(missing(latitude)) stop("must pass latitude")
  if(missing(date.time)) stop("must pass date.time")
  if(missing(labels)) stop("must pass labels")
  if(missing(size)) stop("must pass size")
  if(missing(colour)) stop("must pass colour")
  
  df <- df[,c(longitude, latitude, date.time, labels, size, colour)]
  names(df) <- c("longitude", "latitude", "date_time", "labels", "size",
                 "colour")
  
  # Remove unknown locations
  df <- df[!is.na(df$longitude),]
  
  # Change long & lat class
  df$longitude <- as.numeric(df$longitude)
  df$latitude <- as.numeric(df$latitude)
  
  # format the time column:
  df$date_time <- as.POSIXct(df$date_time, format="%Y-%m-%dT%H:%M:%SZ")
  
  # take unique
  df <- df[!duplicated(df[,c("longitude", "latitude", "labels")]),]
  
  # create a STIDF object:
  sp <- sp::SpatialPoints(df[,c("longitude","latitude")])
  sp::proj4string(sp) <- sp::CRS("+proj=longlat +datum=WGS84")
  
  tweets2earth <- spacetime::STIDF(sp, time = df$date_time, 
                                   data = df[,c("labels","size", "colour")])
  
  # write to a KML file:
  shape <- shape
  
  plotKML::kml(tweets2earth, dtime = 24*3600, size = size, shape = shape, 
               labels = labels, colour = colour, kmz = kmz, 
               file.name = file.name, folder.name = folder.name)
  
  # opens KML file on Google Earth 
  if(open == TRUE) system("open tweets2earth.kml")
  
  return(tweets2earth)
  
}