#' \code{plot2Earth}
#' 
#' @description
#' 
#' Plots tweets location and created time on Google Earth.
#' 
#' @param df, require. data.frame of tweets to plot.
#' @param longitude Required. Column holding longitude.
#' @param latitude Required. Column holding latitude.
#' @param date_time Required. Column including creation time of tweets; POSIXct format
#' @param shape, optional. Shape to use for points.
#' @param labels Required. Column holding labels.
#' @param size Required. Column holding desired size of points.  
#' @param colour, optional. Column name that determines colours of points.
#' @param colour_scale, optional, defaults to SAGA_pal[["SG_COLORS_GREEN_BLACK"]]. Pallettes from R_pal and SAGA_pal. data(R_pal)
#' @param open, optional. Whether to open file in Google Earth. Defaults to TRUE.
#' @param ... Arguments to pass to kml() function.
#' 
#' @details
#' 
#' Takes data.frame of tweets and plots coordinates on Google Earth, also includes created time. Choose size, color and labels of your liking.
#' 
#' @return 
#' 
#' Writes kml files named "tweets2Earth" in working directory and opens file in Google Earth depending on open param.
#' 
#' @examples
#' 
#' \dontrun{
#' library(twitteR)
#' 
#' # setup oauth 
#' setup_twitter_oauth(consumer_key, consumer_secret, access_token=your_token, access_secret=your_secret)
#' 
#' # search twitter
#' tweets <- searchTwitter("searchTerm", n = 2000, lang= "en")
#' 
#' # unlist
#' data_frame <- do.call("rbind", lapply(tweets, as.data.frame))
#' 
#' # inspect
#' names(data_frame)
#' [1] "text"          "favorited"     "favoriteCount" "replyToSN"     "created"      
#' [6] "truncated"     "replyToSID"    "id"            "replyToUID"    "statusSource" 
#' [11] "screenName"    "retweetCount"  "isRetweet"     "retweeted"     "longitude"    
#' [16] "latitude"  
#' 
#' plot2Earth(data_frame, "longitude", "latitude", "created", "screenName", "retweetCount", "favorited")
#' }
plot2Earth <- function(df, longitude, latitude, date_time, labels, size, colour,
                       colour_scale = SAGA_pal[["SG_COLORS_GREEN_BLACK"]], 
                       shape = "http://maps.google.com/mapfiles/kml/pal2/icon18.png", open = TRUE, 
                       ...) {
  
  df <- df[,c(longitude, latitude, date_time, labels, size, colours)]
  names(df) <- c("longitude", "latitude", "date_time", "labels", "size", "colour")
  
  # Remove unknown locations
  df <- df[!is.na(df$longitude),]
  
  # Change long & lat class
  df$longitude <- as.numeric(df$longitude)
  df$latitude <- as.numeric(df$latitude)
  
  # Prepare plot
  sp <- SpatialPoints(df[,c("longitude","latitude")])
  proj4string(sp) <- CRS("+proj=longlat +datum=WGS84")
  tweets2Earth <- STIDF(sp, time = df$date_time, data = df[,c("labels","size")])
  
  # get icon
  shape = "http://maps.google.com/mapfiles/kml/pal2/icon18.png"
  
  kml(tweets2Earth, dtime = 24*3600, size = df$size, shape = shape, colours = colour,
      labels = labels, colour_scale = SAGA_pal[["SG_COLORS_GREEN_BLACK"]], ...)
  if(open == TRUE) {
    system("open tweets2Earth.kml") #Opens KML file on Google Earth 
  } else {
    
  }
  
  return(df_st)
  
}
#' \code{stream2Earth}
#' 
#' @description
#' 
#' Streams tweets' location and time to Google Earth.
#' 
#' @param search_term, required. Search to use in API call.
#' @param oauth, required. Your oauth token as used in streamR package. See ROauth package.
#' @param duration, requried. Number of days to stream, defaults to 1.
#' @param window, required. Duration of the streaming window in seconds. Defaults to 5.
#' @param labels, requried. Labels to use on kml.
#' @param shape, required. Shape to use on Google Earth, has default.
#' @param size, required. Size of shape.
#' @param ... Optional arguments to pass to streamR filterStream function.
#' 
#' @details
#' Streams location and created time onto Google Earth. 
#' 
#' 
#' @return Opens Google Earth and plots tweet location and created time in near-real time.
#' 
#' @examples
#' \dontrun{
#' # OAuth setup - from App (http://apps.twitter.com/)
#' requestURL <- "https://api.twitter.com/oauth/request_token"
#' accessURL <- "https://api.twitter.com/oauth/access_token"
#' authURL <- "https://api.twitter.com/oauth/authorize"
#' my_oauth <- OAuthFactory$new(consumerKey = "your_consumer_key", consumerSecret = "your_consumer_secret",
#'                              requestURL = requestURL, accessURL = accessURL, authURL = authURL)
#' 
#' # download cacert.pem
#' download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")
#' 
#' # Register OAuth
#' my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
#' 
#' #Run stream for 2 days with windows of 7 seconds looking for keyword #rstats
#' stream2Earth("#rstats", oauth, "screen_name", "retweet_count", duration = 2, window = 7)
#' }
#' 
#' @note
#' labels, size parameters must be inherited from tweets filterStream function. 
#' 
#' Available labels and size parameters are;
#' names(filterStream.tweets)
#' [1] "text"                      "retweet_count"            
#' [3] "favorited"                 "truncated"                
#' [5] "id_str"                    "in_reply_to_screen_name"  
#' [7] "source"                    "retweeted"                
#' [9] "created_at"                "in_reply_to_status_id_str"
#' [11] "in_reply_to_user_id_str"   "lang"                     
#' [13] "listed_count"              "verified"                 
#' [15] "location"                  "user_id_str"              
#' [17] "description"               "geo_enabled"              
#' [19] "user_created_at"           "statuses_count"           
#' [21] "followers_count"           "favourites_count"         
#' [23] "protected"                 "user_url"                 
#' [25] "name"                      "time_zone"                
#' [27] "user_lang"                 "utc_offset"               
#' [29] "friends_count"             "screen_name"              
#' [31] "time" 
stream2Earth <- function(search_term, oauth, labels, size, duration = 1,
                         window = 5, shape = "http://maps.google.com/mapfiles/kml/pal2/icon18.png",
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
  end.date <- Sys.Date() + duration
  tw.df <- data.frame()
  while (Sys.Date() < end.date){
    current.time <- format(Sys.time(), "%Y_%m_%d_%H_%M")
    json.file <- paste(search_term, "_", current.time, ".json", sep="")
    filterStream(file=json.file, track=search_term, oauth=oauth, timeout=window, ...)
if (file.info(json.file)$size > 0) {
  df <- parseTweets(json.file)
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
  sp <- SpatialPoints(tw.df[,c("place_lon","place_lat")])
  proj4string(sp) <- CRS("+proj=longlat +datum=WGS84")
  df_st <- STIDF(sp, time = tw.df$time, data = tw.df[,c("labels","size")])
  plotKML(df_st, dtime = 24*3600, points_names=tw.df$labels, LabelScale = .4)
} else {
  Sys.sleep(1)
  print("no geo-located tweets found, restarting stream")
}
  }
}