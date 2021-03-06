% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stream2earth.R
\name{stream2earth}
\alias{stream2earth}
\title{stream2earth}
\usage{
stream2earth(search_term, oauth, labels, size, duration = 3600, window = 5,
  shape = "http://maps.google.com/mapfiles/kml/pal2/icon18.png", ...)
}
\arguments{
\item{search_term, }{required. Search to use in API call.}

\item{oauth, }{required. Your oauth token as used in streamR package. 
See examples.}

\item{labels, }{requried. Labels to use on Google Earth. must be inherited 
from \code{\link[streamR]{filterStream}} (see details)}

\item{size, }{required. Size of shape. must be inherited from 
\code{\link[streamR]{filterStream}} (see details).}

\item{duration, }{requried. Number of seconds to stream, defaults to 
\code{3600}.}

\item{window, }{required. Duration of each streaming window in seconds. 
Defaults to \code{5}.}

\item{shape, }{required. Shape to use on Google Earth. Defaults to 
\code{.../icon18.png}.}

\item{...}{Optional arguments to pass to streamR filterStream function.}
}
\value{
Opens Google Earth and plots tweet location and created time in 
near-real time.
}
\description{
Streams tweets' location and time to Google Earth.
}
\details{
\code{labels}, \code{size} 
parameters must be inherited from \code{\link[streamR]{filterStream}}. 

Available labels and size parameters are;

\itemize{
\item \code{text}
\item \code{favorited}
\item \code{id_str}
\item \code{source}
\item \code{created_at}
\item \code{in_reply_to_user_id_str}
\item \code{listed_count}
\item \code{location}
\item \code{description}
\item \code{user_created_at}
\item \code{followers_count}
\item \code{protected}
\item \code{names}
\item \code{user_lang}
\item \code{friends_count}
\item \code{time}
\item \code{retweet_count}
\item \code{truncated}
\item \code{in_reply_to_screen_name}
\item \code{retweeted}
\item \code{in_reply_to_status_id_str}
\item \code{lang}
\item \code{verified}
\item \code{user_id_str}
\item \code{geo_enabled}
\item \code{statuses_count}
\item \code{favourites_count}
\item \code{user_url}
\item \code{time_zone}
\item \code{utc_offset}
\item \code{screen_name}
}
}
\examples{
\dontrun{
# OAuth setup - from App (http://apps.twitter.com/)
requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
my_oauth <- OAuthFactory$new(consumerKey = "your_consumer_key", 
                             consumerSecret = "your_consumer_secret",
                             requestURL = requestURL, accessURL = accessURL, 
                             authURL = authURL)

# download cacert.pem
download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")

# Register OAuth
my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", 
                                        package = "RCurl"))

#Run stream for 2 days with windows of 7 seconds looking for keyword #rstats
stream2Earth("rstats", my_oauth, "screen_name", "retweet_count", 
             duration = 2, window = 7)
}

}
\author{
Jabber Cruncher, \email{john.coene@jabber-cruncher.com}
}
\seealso{
\code{\link{plot2earth}}
}

