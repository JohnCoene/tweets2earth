#' stream2earth
#' 
#' @description
#' 
#' Streams tweets' location and time to Google Earth.
#' 
#' @param search_term, required. Search to use in API call.
#' @param oauth, required. Your oauth token as used in streamR package. 
#' See examples.
#' @param duration, requried. Number of seconds to stream, defaults to 
#' \code{3600}.
#' @param window, required. Duration of each streaming window in seconds. 
#' Defaults to \code{5}.
#' @param labels, requried. Labels to use on Google Earth. must be inherited 
#' from \code{\link[streamR]{filterStream}} (see details)
#' @param shape, required. Shape to use on Google Earth. Defaults to 
#' \code{.../icon18.png}.
#' @param size, required. Size of shape. must be inherited from 
#' \code{\link[streamR]{filterStream}} (see details).
#' @param ... Optional arguments to pass to streamR filterStream function.
#' 
#' @return Opens Google Earth and plots tweet location and created time in 
#' near-real time.
#' 
#' @examples
#' \dontrun{
#' # OAuth setup - from App (http://apps.twitter.com/)
#' requestURL <- "https://api.twitter.com/oauth/request_token"
#' accessURL <- "https://api.twitter.com/oauth/access_token"
#' authURL <- "https://api.twitter.com/oauth/authorize"
#' my_oauth <- OAuthFactory$new(consumerKey = "your_consumer_key", 
#'                              consumerSecret = "your_consumer_secret",
#'                              requestURL = requestURL, accessURL = accessURL, 
#'                              authURL = authURL)
#' 
#' # download cacert.pem
#' download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")
#' 
#' # Register OAuth
#' my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", 
#'                                         package = "RCurl"))
#' 
#' #Run stream for 2 days with windows of 7 seconds looking for keyword #rstats
#' stream2Earth("rstats", my_oauth, "screen_name", "retweet_count", 
#'              duration = 2, window = 7)
#' }
#' 
#' @details
#' \code{labels}, \code{size} 
#' parameters must be inherited from \code{\link[streamR]{filterStream}}. 
#' 
#' Available labels and size parameters are;
#' 
#' \itemize{
#' \item \code{text}
#' \item \code{favorited}
#' \item \code{id_str}
#' \item \code{source}
#' \item \code{created_at}
#' \item \code{in_reply_to_user_id_str}
#' \item \code{listed_count}
#' \item \code{location}
#' \item \code{description}
#' \item \code{user_created_at}
#' \item \code{followers_count}
#' \item \code{protected}
#' \item \code{names}
#' \item \code{user_lang}
#' \item \code{friends_count}
#' \item \code{time}
#' \item \code{retweet_count}
#' \item \code{truncated}
#' \item \code{in_reply_to_screen_name}
#' \item \code{retweeted}
#' \item \code{in_reply_to_status_id_str}
#' \item \code{lang}
#' \item \code{verified}
#' \item \code{user_id_str}
#' \item \code{geo_enabled}
#' \item \code{statuses_count}
#' \item \code{favourites_count}
#' \item \code{user_url}
#' \item \code{time_zone}
#' \item \code{utc_offset}
#' \item \code{screen_name}
#' }
#' 
#' @author Jabber Cruncher, \email{john.coene@@jabber-cruncher.com}
#' 
#' @seealso \code{\link{plot2earth}}
#' 
#' @export
stream2earth <- function(search_term, oauth, labels, size, duration = 3600,
                         window = 5, 
                         shape = "http://maps.google.com/mapfiles/kml/pal2/icon18.png",
                         ...) {
  if (class(oauth) != "OAuth") {
    stop("oauth not of class OAuth")
  } else if (class(duration) != "numeric") {
    stop ("Duration parameter not of class numeric")
  } else if (class(window) != "numeric"){
    stop ("window must be of class numeric")
  } else if (missing(search_term)) {
    stop ("missing search_term")
  } else if (missing(oauth)) {
    stop("missing oauth")
  }
  end.date <- Sys.time() + duration
  tw.df <- data.frame()
  
  while (Sys.time() < end.date){
    current.time <- format(Sys.time(), "%Y_%m_%d_%H_%M")
    json.file <- paste(search_term, "_", current.time, ".json", sep="")
    streamR::filterStream(file=json.file, track=search_term, oauth=oauth, 
                          timeout=window, ...)
    
    if (file.info(json.file)$size > 0) {
      
      df <- streamR::parseTweets(json.file)
      df <- df[complete.cases(df), ]
      tw.df <- as.data.frame(rbind(tw.df, df))
      
    } else {
      
      print ("no tweets found")
      
      }
    
    if (length(df$place_lon) >= 1) {
      # build df
      tw.df <- tw.df[,c("place_lon", "place_lat", "created_at", size, labels)]
      names(tw.df) <- c("place_lon", "place_lat", "created_at","size", "labels")
      
      # get shape
      shape <- shape
      
      # fix time
      tw.df$time <- substr(tw.df$created_at, 12, 19)
      tw.df$time <- as.POSIXct(strptime(tw.df$time, "%H:%M:%S"))
      
      # plot
      sp <- sp::SpatialPoints(tw.df[,c("place_lon","place_lat")])
      sp::proj4string(sp) <- sp::CRS("+proj=longlat +datum=WGS84")
      df_st <- spacetime::STIDF(sp, time = tw.df$time, 
                                data = tw.df[,c("labels","size")])
      
      plotKML::plotKML(df_st, dtime = 24*3600, points_names=tw.df$labels, 
              LabelScale = .6)
    } else {
      
      Sys.sleep(0.5)
      print("no geo-located tweets found, restarting stream")
      
    }
  }
}